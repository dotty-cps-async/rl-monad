package cps.learning.backends.jql

import ai.djl.Model
import ai.djl.inference.Predictor
import ai.djl.ndarray.{NDArray, NDList}
import ai.djl.ndarray.types.Shape
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
class DJLRLModel[S: NDArrayRepresentation, A: IntRepresentation] private(
                                                                          name: String,
                                                                          qNetwork: Model,
                                                                          qNetworkPredictor: Predictor[NDArray, NDArray],
                                                                          targetNetwork: Model,
                                                                          targetNetworkTrainer: Trainer,
                                                                          random: Random = new Random(),
                                                                          epsilon: Float = 0.1f,
                                                                        ) extends RLModel[S, A, Float] {


  // we assume that qNetwork return q-values for all actions in one dimensions.
  override def maxPossibleAction: Int = {
    qNetwork.describeOutput().get(0).getValue.size().toInt
  }

  override def predictQ(input: S, action: A): Float = {
    val inputNDArray = summon[NDArrayRepresentation[S]].toNDArray(input)
    val actionIndex = summon[IntRepresentation[A]].toInt(action)
    val qValues = qNetworkPredictor.predict(inputNDArray)
    qValues.getFloat(actionIndex)
  }

  override def selectOne(state: S, validActions: IndexedSeq[A], mode: AgentRunningMode): A = {
    val index =
      if (mode == Explore && random.nextFloat < epsilon) then {
        // Explore: random valid move
        random.nextInt(validActions.size)
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

  override def trainCase(state: S, nextState: S, target: A, reward: Float): RLModel[S, A, Float] = {
    ???

  }


}

object DJLRLModel {

  def apply[S: NDArrayRepresentation, A: IntRepresentation](
                                                             name: String,
                                                             qBuilder: DJLNNBuilder,
                                                             inputShape: Shape,
                                                             random: Random,
                                                             epsilon0: Float = 0.1f
                                                           ): DJLRLModel[S, A] = {

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