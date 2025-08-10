package cps.learning.backends.jql

import ai.djl.Model
import ai.djl.inference.Predictor
import ai.djl.ndarray.{NDArray, NDList}
import ai.djl.ndarray.types.Shape
import ai.djl.training.dataset.Batch
import ai.djl.training.loss.Loss
import ai.djl.training.optimizer.Optimizer
import ai.djl.training.{DefaultTrainingConfig, Trainer}
import cps.*
import cps.learning.*
import cps.learning.AgentRunningMode.Explore

import scala.util.Random


/**
 * Local model, which is running locally via DJL.
 */
case class DJLRLImmutableModel[S: NDArrayRepresentation, A: IntRepresentation] private(
                                                                                        name: String,
                                                                                        qNetwork: Model,
                                                                                        qNetworkPredictor: Predictor[NDArray, NDArray],
                                                                                        targetNetwork: Model,
                                                                                        targetNetworkTrainer: Trainer,
                                                                                        params: DJLRLModelParams,
                                                                                        replayBuffer: Vector[Experience[S, A]] = Vector.empty,
                                                                                        nStepsAfterTraining: Int = 0,
                                                                                      ) extends RLImmutableModel[S, A, Float] {


  // we assume that qNetwork return q-values for all actions in one dimensions.
  override def maxPossibleAction: Int = {
    qNetwork.describeOutput().get(0).getValue.size().toInt
  }


  override def selectOne(state: S, validActions: IndexedSeq[A], mode: AgentRunningMode): A = {
    val index =
      if (mode == Explore && params.random.nextFloat < params.epsilon) then {
        // Explore: random valid move
        params.random.nextInt(validActions.size)
      } else
        // Exploit: choose best action
        val input = summon[NDArrayRepresentation[S]].toNDArray(state)
        val qValues = qNetworkPredictor.predict(input)
        var bestActionIndex = -1
        var bestValue = Float.NegativeInfinity
        for (actionIndex <- validActions.indices) {
          val value = qValues.getFloat(actionIndex)
          if (value > bestValue) {
            bestValue = value
            bestActionIndex = actionIndex
          }
        }
        bestActionIndex
    summon[IntRepresentation[A]].fromTensor(index).getOrElse(
      throw new IllegalStateException(s"Invalid action index in the output of the model: $index for valid actions: $validActions")
    )
  }

  override def trainCase(state: S, nextState: S, action: A, reward: Float, finish: Boolean): RLImmutableModel[S, A, Float] = {
    val experience = Experience(state, action, nextState, reward, finish)
    val newReplayBuffer = (replayBuffer :+ experience).takeRight(ReplayBuffer.DEFAULT_MAX_SIZE)
    if (newReplayBuffer.length >= params.minBatchSize && nStepsAfterTraining >= params.nStepsBetweenTraining) {
      trainBatch(newReplayBuffer)
    } else {
      copy(replayBuffer = newReplayBuffer, nStepsAfterTraining = nStepsAfterTraining + 1)
    }
  }

  def trainBatch(newReplayBuffer: Vector[Experience[S, A]]): DJLRLImmutableModel[S, A] = {

    val sample = takeSample(newReplayBuffer, params.minBatchSize, params.random)

    //val stateArray = new Array[Array[Float]](params.minBatchSize, params.stateSize)
    ???

  }
}

object DJLRLImmutableModel {

  def create[S: NDArrayRepresentation, A: IntRepresentation](
                                                             name: String,
                                                             qBuilder: DJLNNBuilder,
                                                             inputShape: Shape,
                                                             random: Random,
                                                             epsilon0: Float = 0.1f
                                                           ): DJLRLImmutableModel[S, A] = {

    val qNetwork = Model.newInstance(s"${name}-Q")
    qNetwork.setBlock(qBuilder())
    val targetNetwork = Model.newInstance(s"${name}-target")
    targetNetwork.setBlock(qBuilder())

    val config = new DefaultTrainingConfig(Loss.l2Loss())
      .optOptimizer(Optimizer.adam().build());

    val trainer = targetNetwork.newTrainer(config)
    trainer.initialize(inputShape);

    var epsilon = epsilon0

    ???
  }

}