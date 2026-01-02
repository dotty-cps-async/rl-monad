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


case class DJRLModelState[S, A](
                                 qNetwork: Model,
                                 qNetworkTrainer: Trainer,
                                 qNetworkPredictor: Predictor[NDArray, NDArray],
                                 targetNetwork: Model,
                                 targetNetworkPredictor: Predictor[NDArray, NDArray],
                                 replayBuffer: Vector[Experience[S, A]],
                                 nStepsAfterTraining: Int,
                                 totalTrainingSteps: Int,
                               )

/**
 * Local model, which is running locally via DJL.
 */
class DJLRLModelControl[F[_] : CpsScoredLogicMonad.Curry[Float], S: NDArrayRepresentation, A: IntRepresentation](params: DJLRLModelParams) extends RLModelControl[F, S, A, Float, DJRLModelState[S, A]] {


  override def initialModel: DJRLModelState[S, A] = {
    val qNetwork = Model.newInstance(s"${params.name}-Q")
    qNetwork.setBlock(params.qBuilder())

    val targetNetwork = Model.newInstance(s"${params.name}-target")
    targetNetwork.setBlock(params.qBuilder())

    val config = new DefaultTrainingConfig(Loss.l2Loss())
      .optOptimizer(Optimizer.adam().build())

    val trainer = qNetwork.newTrainer(config)
    trainer.initialize(new Shape(params.minBatchSize, params.stateSize))

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

  override def maxPossibleActons(model: DJRLModelState[S, A]): Int = params.actionSize


  override def rateActions(modelState: DJRLModelState[S, A], envState: S, validActions: IndexedSeq[A], mode: AgentRunningMode): F[A] = {
    val scored =
      if (mode == Explore && params.random.nextFloat < params.epsilon) then
        // Explore: random valid move
        validActions.zipWithIndex.map { (action, actionIndex) =>
          val value = params.random.nextFloat
          (value, action)
        }
      else
        // Exploit: choose best action
        val input = summon[NDArrayRepresentation[S]].toNDArray(envState)
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

  override def trainCase(modelState: DJRLModelState[S, A], envState: S, nextState: S, action: A, reward: Float, finish: Boolean): F[DJRLModelState[S, A]] = {
    val experience = Experience(envState, action, nextState, reward, finish)
    val newReplayBuffer = (modelState.replayBuffer :+ experience).takeRight(ReplayBuffer.DEFAULT_MAX_SIZE)
    val nextModelState = if (newReplayBuffer.length >= params.minBatchSize && modelState.nStepsAfterTraining >= params.nStepsBetweenTraining) {
      trainBatch(modelState, newReplayBuffer)
    } else {
      modelState.copy(replayBuffer = newReplayBuffer, nStepsAfterTraining = modelState.nStepsAfterTraining + 1)
    }
    summon[CpsScoredLogicMonad[F, Float]].pure(nextModelState)
  }

  def trainBatch(modelState: DJRLModelState[S, A], newReplayBuffer: Vector[Experience[S, A]]): DJRLModelState[S, A] = {
    val sample = takeSample(newReplayBuffer, params.minBatchSize, params.random)
    val stateRepr = summon[NDArrayRepresentation[S]]
    val actionRepr = summon[IntRepresentation[A]]
    val manager = modelState.qNetworkTrainer.getManager

    // Build batch arrays
    val statesData = new Array[Float](params.minBatchSize * params.stateSize)
    val nextStatesData = new Array[Float](params.minBatchSize * params.stateSize)
    val rewards = new Array[Float](params.minBatchSize)
    val actions = new Array[Int](params.minBatchSize)
    val dones = new Array[Float](params.minBatchSize)

    for (i <- sample.indices) {
      val exp = sample(i)
      val stateArray = stateRepr.toNDArray(exp.state).toFloatArray
      val nextStateArray = stateRepr.toNDArray(exp.nextState).toFloatArray
      System.arraycopy(stateArray, 0, statesData, i * params.stateSize, params.stateSize)
      System.arraycopy(nextStateArray, 0, nextStatesData, i * params.stateSize, params.stateSize)
      rewards(i) = exp.reward
      actions(i) = actionRepr.toInt(exp.action)
      dones(i) = if (exp.done) 1.0f else 0.0f
    }

    // Create NDArrays
    val statesND = manager.create(statesData, new Shape(params.minBatchSize, params.stateSize))
    val nextStatesND = manager.create(nextStatesData, new Shape(params.minBatchSize, params.stateSize))
    val rewardsND = manager.create(rewards)
    val donesND = manager.create(dones)

    // Compute target Q values: reward + gamma * max(Q_target(nextState)) * (1 - done)
    val nextQValues = modelState.targetNetworkPredictor.predict(nextStatesND)
    val maxNextQ = nextQValues.max(Array(1))
    val targets = rewardsND.add(maxNextQ.mul(params.gamma).mul(donesND.neg().add(1.0f)))

    // Get current Q values and update only the selected actions
    val currentQValues = modelState.qNetworkPredictor.predict(statesND)
    val targetQValues = currentQValues.duplicate()

    for (i <- 0 until params.minBatchSize) {
      targetQValues.set(new NDIndex(i, actions(i)), targets.getFloat(i))
    }

    // Train
    val gc = modelState.qNetworkTrainer.newGradientCollector()
    try {
      val predictions = modelState.qNetworkTrainer.forward(new NDList(statesND)).singletonOrThrow()
      val loss = Loss.l2Loss().evaluate(new NDList(targetQValues), new NDList(predictions))
      gc.backward(loss)
      modelState.qNetworkTrainer.step()
    } finally {
      gc.close()
    }

    val newTotalSteps = modelState.totalTrainingSteps + 1

    // Periodically update target network
    val updatedTargetPredictor = if (newTotalSteps % params.targetUpdateFrequency == 0) {
      copyModelParameters(modelState.qNetwork, modelState.targetNetwork, manager)
      modelState.targetNetwork.newPredictor(NDArrayTranslator)
    } else {
      modelState.targetNetworkPredictor
    }

    modelState.copy(
      targetNetworkPredictor = updatedTargetPredictor,
      replayBuffer = newReplayBuffer,
      nStepsAfterTraining = 0,
      totalTrainingSteps = newTotalSteps
    )
  }

  private def copyModelParameters(source: Model, target: Model, manager: NDManager): Unit = {
    val sourceParams = source.getBlock.getParameters
    val targetParams = target.getBlock.getParameters
    sourceParams.forEach { entry =>
      val name = entry.getKey
      val sourceParam = entry.getValue
      val targetParam = targetParams.get(name)
      if (targetParam != null && sourceParam.getArray != null) {
        targetParam.setArray(sourceParam.getArray.duplicate())
      }
    }
  }
}

object NDArrayTranslator extends ai.djl.translate.Translator[NDArray, NDArray] {
  override def processInput(ctx: ai.djl.translate.TranslatorContext, input: NDArray): NDList = new NDList(input)
  override def processOutput(ctx: ai.djl.translate.TranslatorContext, list: NDList): NDArray = list.singletonOrThrow()
}

