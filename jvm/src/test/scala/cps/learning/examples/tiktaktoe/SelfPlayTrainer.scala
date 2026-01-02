package cps.learning.examples.tiktaktoe

import scala.util.Random
import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.nn.Block
import ai.djl.Model
import ai.djl.inference.Predictor
import ai.djl.training.{DefaultTrainingConfig, Trainer}
import ai.djl.training.loss.Loss
import ai.djl.training.optimizer.Optimizer
import ai.djl.ndarray.types.Shape
import ai.djl.ndarray.index.NDIndex

import cps.learning.*
import cps.learning.backends.jql.*

case class SelfPlayConfig(
  boardSize: Int = 5,
  winLength: Int = 4,
  numEpisodes: Int = 500,
  epsilon: Float = 0.3f,
  epsilonDecay: Float = 0.995f,
  minEpsilon: Float = 0.05f,
  gamma: Float = 0.99f,
  batchSize: Int = 32,
  replayBufferSize: Int = 10000,
  nStepsBetweenTraining: Int = 10,
  targetUpdateFrequency: Int = 100,
  random: Random = new Random()
)

case class TrainingMetrics(
  episode: Int,
  player1Wins: Int,
  player2Wins: Int,
  draws: Int,
  avgGameLength: Double
) {
  def winRate(player: Int): Double = {
    val total = player1Wins + player2Wins + draws
    if total == 0 then 0.0
    else if player == 1 then player1Wins.toDouble / total
    else player2Wins.toDouble / total
  }
}

case class SimpleExperience(
  observation: Array[Float],
  action: Int,
  nextObservation: Array[Float],
  reward: Float,
  done: Boolean
)

/**
 * A simpler self-play trainer that doesn't use the scored logic monad directly,
 * but implements DQN training in an imperative style.
 */
class SelfPlayTrainer(config: SelfPlayConfig)(using ndManager: NDManager) {

  val game = new TikTakToeGame(config.boardSize, config.winLength)
  val observationSize = config.boardSize * config.boardSize
  val actionSize = config.boardSize * config.boardSize

  // Neural network setup
  val qNetwork: Model = Model.newInstance("tiktaktoe-q")
  qNetwork.setBlock(DQNBoardModel.buildBlock(config.boardSize))

  val targetNetwork: Model = Model.newInstance("tiktaktoe-target")
  targetNetwork.setBlock(DQNBoardModel.buildBlock(config.boardSize))

  val trainingConfig = new DefaultTrainingConfig(Loss.l2Loss())
    .optOptimizer(Optimizer.adam().build())

  val trainer: Trainer = qNetwork.newTrainer(trainingConfig)
  trainer.initialize(new Shape(config.batchSize, observationSize))

  val qPredictor: Predictor[NDArray, NDArray] = qNetwork.newPredictor(NDArrayTranslator)
  var targetPredictor: Predictor[NDArray, NDArray] = targetNetwork.newPredictor(NDArrayTranslator)

  // Replay buffer
  var replayBuffer: Vector[SimpleExperience] = Vector.empty
  var totalTrainingSteps: Int = 0
  var stepsAfterTraining: Int = 0

  def boardToArray(board: Board): Array[Float] = {
    val arr = new Array[Float](observationSize)
    for (i <- 0 until config.boardSize; j <- 0 until config.boardSize) {
      arr(i * config.boardSize + j) = board.getOrElse((i, j), 0).toFloat
    }
    arr
  }

  def possibleActions(state: GameState): IndexedSeq[Int] = {
    (for {
      i <- 0 until config.boardSize
      j <- 0 until config.boardSize
      if state.board.isEmpty(i, j)
    } yield i * config.boardSize + j).toIndexedSeq
  }

  def actionToMove(actionIdx: Int, player: Int): Move = {
    Move(actionIdx / config.boardSize, actionIdx % config.boardSize, player)
  }

  def selectAction(observation: Array[Float], validActions: IndexedSeq[Int], epsilon: Float): Int = {
    if (config.random.nextFloat < epsilon) {
      // Explore: random action
      validActions(config.random.nextInt(validActions.size))
    } else {
      // Exploit: best action from Q-network
      val input = ndManager.create(observation)
      val qValues = qPredictor.predict(input)
      validActions.maxBy(a => qValues.getFloat(a))
    }
  }

  def addExperience(exp: SimpleExperience): Unit = {
    replayBuffer = (replayBuffer :+ exp).takeRight(config.replayBufferSize)
  }

  def trainBatch(): Unit = {
    if (replayBuffer.size < config.batchSize) return

    val sample = config.random.shuffle(replayBuffer.indices.toList).take(config.batchSize).map(replayBuffer(_))

    val statesData = new Array[Float](config.batchSize * observationSize)
    val nextStatesData = new Array[Float](config.batchSize * observationSize)
    val rewards = new Array[Float](config.batchSize)
    val actions = new Array[Int](config.batchSize)
    val dones = new Array[Float](config.batchSize)

    for (i <- sample.indices) {
      val exp = sample(i)
      System.arraycopy(exp.observation, 0, statesData, i * observationSize, observationSize)
      System.arraycopy(exp.nextObservation, 0, nextStatesData, i * observationSize, observationSize)
      rewards(i) = exp.reward
      actions(i) = exp.action
      dones(i) = if (exp.done) 1.0f else 0.0f
    }

    val manager = trainer.getManager
    val statesND = manager.create(statesData, new Shape(config.batchSize, observationSize))
    val nextStatesND = manager.create(nextStatesData, new Shape(config.batchSize, observationSize))
    val rewardsND = manager.create(rewards)
    val donesND = manager.create(dones)

    // Compute target Q values
    val nextQValues = targetPredictor.predict(nextStatesND)
    val maxNextQ = nextQValues.max(Array(1))
    val targets = rewardsND.add(maxNextQ.mul(config.gamma).mul(donesND.neg().add(1.0f)))

    // Get current Q values and update selected actions
    val currentQValues = qPredictor.predict(statesND)
    val targetQValues = currentQValues.duplicate()

    for (i <- 0 until config.batchSize) {
      targetQValues.set(new NDIndex(i, actions(i)), targets.getFloat(i))
    }

    // Train
    val gc = trainer.newGradientCollector()
    try {
      val predictions = trainer.forward(new ai.djl.ndarray.NDList(statesND)).singletonOrThrow()
      val loss = Loss.l2Loss().evaluate(new ai.djl.ndarray.NDList(targetQValues), new ai.djl.ndarray.NDList(predictions))
      gc.backward(loss)
      trainer.step()
    } finally {
      gc.close()
    }

    totalTrainingSteps += 1

    // Periodically update target network
    if (totalTrainingSteps % config.targetUpdateFrequency == 0) {
      copyModelParameters(qNetwork, targetNetwork, manager)
      targetPredictor = targetNetwork.newPredictor(NDArrayTranslator)
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
        targetParam.setArray(sourceParam.getArray.duplicate())
      }
    }
  }

  def train(): TrainingMetrics = {
    var player1Wins = 0
    var player2Wins = 0
    var draws = 0
    var totalGameLength = 0
    var epsilon = config.epsilon

    for (episode <- 1 to config.numEpisodes) {
      var state = game.initState
      var stepCount = 0

      while (!game.isFinalState(state)) {
        val observation = boardToArray(game.observe(state))
        val validActions = possibleActions(state)

        if (validActions.nonEmpty) {
          val actionIdx = selectAction(observation, validActions, epsilon)
          val move = actionToMove(actionIdx, state.nextPlayer)

          game.applyAction(state, move) match {
            case Some((nextState, reward)) =>
              val nextObservation = boardToArray(game.observe(nextState))
              val done = game.isFinalState(nextState)

              addExperience(SimpleExperience(observation, actionIdx, nextObservation, reward, done))

              stepsAfterTraining += 1
              if (stepsAfterTraining >= config.nStepsBetweenTraining) {
                trainBatch()
                stepsAfterTraining = 0
              }

              state = nextState
              stepCount += 1

            case None =>
              // Invalid action - shouldn't happen
              stepCount += 1
          }
        } else {
          stepCount += 1
        }
      }

      // Record game result
      totalGameLength += stepCount
      state.board.winner match {
        case Some(1) => player1Wins += 1
        case Some(2) => player2Wins += 1
        case Some(_) => draws += 1 // shouldn't happen with valid players
        case None => draws += 1
      }

      // Decay epsilon
      epsilon = math.max(config.minEpsilon, epsilon * config.epsilonDecay).toFloat

      // Log progress periodically
      if (episode % 50 == 0) {
        val avgLen = totalGameLength.toDouble / episode
        println(f"Episode $episode: P1 wins=$player1Wins, P2 wins=$player2Wins, draws=$draws, avg_len=$avgLen%.1f, epsilon=$epsilon%.3f")
      }
    }

    TrainingMetrics(
      episode = config.numEpisodes,
      player1Wins = player1Wins,
      player2Wins = player2Wins,
      draws = draws,
      avgGameLength = totalGameLength.toDouble / config.numEpisodes
    )
  }

  def close(): Unit = {
    qNetwork.close()
    targetNetwork.close()
    trainer.close()
  }
}

object SelfPlayTrainer {

  def run(config: SelfPlayConfig = SelfPlayConfig()): TrainingMetrics = {
    val ndManager = NDManager.newBaseManager()
    try {
      given NDManager = ndManager
      val trainer = new SelfPlayTrainer(config)
      try {
        trainer.train()
      } finally {
        trainer.close()
      }
    } finally {
      ndManager.close()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting TikTakToe self-play training...")
    val metrics = run()
    println(s"\nTraining complete!")
    println(s"Final metrics: P1 wins=${metrics.player1Wins}, P2 wins=${metrics.player2Wins}, draws=${metrics.draws}")
    println(f"Average game length: ${metrics.avgGameLength}%.1f moves")
  }
}
