package cps.learning.backends.jql

import ai.djl.Model
import ai.djl.inference.Predictor
import ai.djl.ndarray.{NDArray, NDList, NDManager}
import ai.djl.ndarray.index.NDIndex
import ai.djl.ndarray.types.Shape
import ai.djl.training.dataset.Batch
import ai.djl.training.loss.Loss
import ai.djl.training.optimizer.Optimizer
import ai.djl.training.{DefaultTrainingConfig, Trainer}
import cps.*
import cps.learning.*
import cps.learning.AgentRunningMode.Explore

import scala.util.Random
import java.util.concurrent.locks.ReentrantLock


case class DJRLModelState[O, A](
                                 qNetwork: Model,
                                 qNetworkTrainer: Trainer,
                                 qNetworkPredictor: Predictor[NDArray, NDArray],
                                 targetNetwork: Model,
                                 targetNetworkPredictor: Predictor[NDArray, NDArray],
                                 replayBuffer: Vector[Experience[O, A]],
                                 nStepsAfterTraining: Int,
                                 totalTrainingSteps: Int,
                                 trainLock: ReentrantLock = new ReentrantLock()
                               ) {
  /**
   * Close all resources held by this model state.
   * Should be called when training is complete.
   */
  def close(): Unit = {
    qNetworkPredictor.close()
    targetNetworkPredictor.close()
    qNetworkTrainer.close()
    qNetwork.close()
    targetNetwork.close()
  }
}

/**
 * Type alias for TensorRepresentation with NDArray tensor type
 */
type NDArrayTensorRepresentation[A] = TensorRepresentation[A] { type Tensor = NDArray }

/**
 * Local model, which is running locally via DJL.
 */
class DJLRLModelControl[F[_] : CpsScoredLogicMonad.Curry[Float], S, O, A: IntRepresentation](params: DJLRLModelParams)(using obsRepr: TensorRepresentation[O] { type Tensor = NDArray }) extends RLModelControl[F, S, O, A, Float, DJRLModelState[O, A]] {


  override def initialModel: DJRLModelState[O, A] = {
    val qNetwork = Model.newInstance(s"${params.name}-Q")
    qNetwork.setBlock(params.qBuilder())

    val targetNetwork = Model.newInstance(s"${params.name}-target")
    targetNetwork.setBlock(params.qBuilder())

    val config = new DefaultTrainingConfig(Loss.l2Loss())
      .optOptimizer(Optimizer.adam().build())

    val trainer = qNetwork.newTrainer(config)
    val inputShape = new Shape(params.minBatchSize, params.observationSize)
    trainer.initialize(inputShape)

    // Initialize target network with the same shape using the trainer's manager
    // so parameters are not released when we copy them later
    val trainerManager = trainer.getManager
    targetNetwork.getBlock.initialize(trainerManager, ai.djl.ndarray.types.DataType.FLOAT32, inputShape)

    val qPredictor = qNetwork.newPredictor(NDArrayTranslator)
    val targetPredictor = targetNetwork.newPredictor(NDArrayTranslator)

    DJRLModelState(
      qNetwork = qNetwork,
      qNetworkTrainer = trainer,
      qNetworkPredictor = qPredictor,
      targetNetwork = targetNetwork,
      targetNetworkPredictor = targetPredictor,
      replayBuffer = Vector.empty,
      nStepsAfterTraining = 0,
      totalTrainingSteps = 0
    )
  }

  override def maxPossibleActions(model: DJRLModelState[O, A]): Int = params.actionSize


  override def rateActions(modelState: DJRLModelState[O, A], observation: O, validActions: IndexedSeq[A], mode: AgentRunningMode): F[A] = {
    val scored =
      if (mode == Explore && params.random.nextFloat < params.epsilon) then
        // Explore: random valid move
        validActions.zipWithIndex.map { (action, actionIndex) =>
          val value = params.random.nextFloat
          (value, action)
        }
      else
        // Exploit: choose best action
        val input = obsRepr.toTensor(observation)
        val qValues = modelState.qNetworkPredictor.predict(input)

        validActions.zipWithIndex.map { (action, actionIndex) =>
          val value = qValues.getFloat(actionIndex)
          (value, action)
        }


    val retval = summon[CpsScoredLogicMonad[F,Float]].multiScore(scored.map {
      case (score, action) => (score, () => summon[CpsScoredLogicMonad[F, Float]].pure(action))
    })
    retval

  }

  override def trainCase(modelState: DJRLModelState[O, A], observation: O, nextObservation: O, action: A, reward: Float, finish: Boolean): F[DJRLModelState[O, A]] = {
    val experience = Experience(observation, action, nextObservation, reward, finish)
    val newReplayBuffer = (modelState.replayBuffer :+ experience).takeRight(ReplayBuffer.DEFAULT_MAX_SIZE)
    val nextModelState = if (newReplayBuffer.length >= params.minBatchSize && modelState.nStepsAfterTraining >= params.nStepsBetweenTraining) {
      trainBatch(modelState, newReplayBuffer)
    } else {
      modelState.copy(replayBuffer = newReplayBuffer, nStepsAfterTraining = modelState.nStepsAfterTraining + 1)
    }
    summon[CpsScoredLogicMonad[F, Float]].pure(nextModelState)
  }

  def trainBatch(modelState: DJRLModelState[O, A], newReplayBuffer: Vector[Experience[O, A]]): DJRLModelState[O, A] = {
    // Lock to prevent concurrent training on the same model from multiple threads
    modelState.trainLock.lock()
    try {
      val sample = takeSample(newReplayBuffer, params.minBatchSize, params.random)
      val actionRepr = summon[IntRepresentation[A]]
      val manager = modelState.qNetworkTrainer.getManager

      // Build batch arrays
      val obsData = new Array[Float](params.minBatchSize * params.observationSize)
      val nextObsData = new Array[Float](params.minBatchSize * params.observationSize)
      val rewards = new Array[Float](params.minBatchSize)
      val actions = new Array[Int](params.minBatchSize)
      val dones = new Array[Float](params.minBatchSize)

      for (i <- sample.indices) {
        val exp = sample(i)
        val obsArray = obsRepr.toTensor(exp.observation).toFloatArray
        val nextObsArray = obsRepr.toTensor(exp.nextObservation).toFloatArray
        System.arraycopy(obsArray, 0, obsData, i * params.observationSize, params.observationSize)
        System.arraycopy(nextObsArray, 0, nextObsData, i * params.observationSize, params.observationSize)
        rewards(i) = exp.reward
        actions(i) = actionRepr.toInt(exp.action)
        dones(i) = if (exp.done) 1.0f else 0.0f
      }

      // Create NDArrays
      val obsND = manager.create(obsData, new Shape(params.minBatchSize, params.observationSize))
      val nextObsND = manager.create(nextObsData, new Shape(params.minBatchSize, params.observationSize))
      val rewardsND = manager.create(rewards)
      val donesND = manager.create(dones)

      // Compute target Q values: reward + gamma * max(Q_target(nextObs)) * (1 - done)
      val nextQValues = modelState.targetNetworkPredictor.predict(nextObsND)
      val maxNextQ = nextQValues.max(Array(1))
      val targets = rewardsND.add(maxNextQ.mul(params.gamma).mul(donesND.neg().add(1.0f)))

      // Get current Q values and update only the selected actions
      val currentQValues = modelState.qNetworkPredictor.predict(obsND)
      val targetQValues = currentQValues.duplicate()

      for (i <- 0 until params.minBatchSize) {
        targetQValues.set(new NDIndex(i, actions(i)), targets.getFloat(i))
      }

      // Train
      val gc = modelState.qNetworkTrainer.newGradientCollector()
      try {
        val predictions = modelState.qNetworkTrainer.forward(new NDList(obsND)).singletonOrThrow()
        val loss = Loss.l2Loss().evaluate(new NDList(targetQValues), new NDList(predictions))
        gc.backward(loss)
        modelState.qNetworkTrainer.step()
      } finally {
        gc.close()
      }

      val newTotalSteps = modelState.totalTrainingSteps + 1

      // Periodically update target network parameters
      // Note: We don't create a new predictor - the existing predictor will use the updated model
      if (newTotalSteps % params.targetUpdateFrequency == 0) {
        copyModelParameters(modelState.qNetwork, modelState.targetNetwork, manager)
      }

      modelState.copy(
        replayBuffer = newReplayBuffer,
        nStepsAfterTraining = 0,
        totalTrainingSteps = newTotalSteps
      )
    } finally {
      modelState.trainLock.unlock()
    }
  }

  private def copyModelParameters(source: Model, target: Model, manager: NDManager): Unit = {
    val sourceParams = source.getBlock.getParameters
    val targetParams = target.getBlock.getParameters
    sourceParams.forEach { entry =>
      val name = entry.getKey
      val sourceParam = entry.getValue
      val targetParam = targetParams.get(name)
      if (targetParam != null && sourceParam.getArray != null) {
        val sourceArray = sourceParam.getArray
        val targetArray = targetParam.getArray
        // Copy data from source to target by duplicating and copying values in-place
        targetArray.set(sourceArray.toFloatArray)
      }
    }
  }
}

object NDArrayTranslator extends ai.djl.translate.Translator[NDArray, NDArray] {
  override def processInput(ctx: ai.djl.translate.TranslatorContext, input: NDArray): NDList = new NDList(input)
  override def processOutput(ctx: ai.djl.translate.TranslatorContext, list: NDList): NDArray = list.singletonOrThrow()
}

