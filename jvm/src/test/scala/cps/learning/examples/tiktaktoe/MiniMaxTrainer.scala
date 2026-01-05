package cps.learning.examples.tiktaktoe

import scala.util.Random
import java.nio.file.{Path, Paths, Files}
import ai.djl.ndarray.NDManager
import ai.djl.Device
import ai.djl.engine.Engine

import cps.*
import cps.monads.{CpsIdentity, given}
import cps.monads.logic.{*, given}
import cps.learning.*
import cps.learning.backends.jql.*
import cps.learning.ScoredLogicStreamT
import cps.learning.ScoredLogicStreamT.given
import cps.learning.ds.LogicalSearchPolicy.given

case class MiniMaxConfig(
  boardSize: Int = 5,
  winLength: Int = 4,
  maxRecursionDepth: Int = 4,
  maxBranches: Int = 5,  // Limit branching like LogicTree's topN
  numEpisodes: Int = 5000,
  epsilon: Float = 0.2f,
  epsilonDecay: Float = 0.995f,
  minEpsilon: Float = 0.05f,
  gamma: Float = 0.99f,
  batchSize: Int = 32,
  nStepsBetweenTraining: Int = 5,
  targetUpdateFrequency: Int = 50,
  trainingMode: VirtualStepTrainingMode = VirtualStepTrainingMode.OnCommit,
  saveEvery: Int = 500,
  modelPath: Option[String] = Some("models/tiktaktoe-minimax"),
  random: Random = new Random()
)

/**
 * MiniMax agent for TikTakToe using RLMiniMaxAgentBehavior.
 * Just implements the 3 abstract methods.
 */
class TikTakToeMiniMaxAgent[F[_]: CpsScoredLogicMonad.Curry[Float]](
  modelControl: RLModelControl[F, GameState, Board, Move, Float, DJRLModelState[Board, Move]],
  boardSize: Int,
  maxDepth: Int,
  trainingMode: VirtualStepTrainingMode
) extends RLMiniMaxAgentBehavior[F, GameState, Board, Move, DJRLModelState[Board, Move], Float](
  modelControl, maxDepth, trainingMode
) {
  private val ordering = summon[LinearlyOrderedGroup[Float]]

  override def possibleActions(env: RLEnvironment[GameState, Board, Move], state: GameState, agentState: DJRLModelState[Board, Move]): IndexedSeq[Move] = {
    (for {
      i <- 0 until boardSize
      j <- 0 until boardSize
      if state.board.isEmpty(i, j)
    } yield Move(i, j, state.nextPlayer)).toIndexedSeq
  }

  override def gainEstimation(env: RLEnvironment[GameState, Board, Move], state: GameState, reward: Float): Float = {
    // reward > 0 means acting player won, < 0 means loss/draw
    if reward > 0.5f then ordering.maxPositiveValue
    else if reward < -0.5f then 0.0f  // Loss
    else if env.isFinalState(state) then 0.5f  // Draw
    else ordering.one  // Ongoing - neutral gain (multiplicative identity)
  }

  override def lossThreshold: Float = 0.1f
}

/**
 * MiniMax trainer for TikTakToe using symmetric self-play.
 */
class MiniMaxTrainer(config: MiniMaxConfig)(using ndManager: NDManager) {

  type LogicF[A] = ScoredLogicStreamT[CpsIdentity, A, Float]

  val game = new TikTakToeGame(config.boardSize, config.winLength)

  given moveRepr: IntRepresentation[Move] = MoveIntRepresentation(config.boardSize)

  val modelParams = DJLRLModelParams(
    name = "tiktaktoe-minimax",
    qBuilder = () => DQNBoardModel.buildBlock(config.boardSize),
    observationSize = config.boardSize * config.boardSize,
    actionSize = config.boardSize * config.boardSize,
    epsilon = config.epsilon,
    gamma = config.gamma,
    minBatchSize = config.batchSize,
    nStepsBetweenTraining = config.nStepsBetweenTraining,
    targetUpdateFrequency = config.targetUpdateFrequency,
    random = config.random
  )

  val modelControl = new DJLRLModelControl[LogicF, GameState, Board, Move](modelParams)

  /**
   * Save model to file.
   */
  def saveModel(model: DJRLModelState[Board, Move], path: String): Unit = {
    val dir = Paths.get(path).getParent
    if (dir != null && !Files.exists(dir)) {
      Files.createDirectories(dir)
    }
    model.qNetwork.save(Paths.get(path), "q-network")
    println(s"Model saved to: $path")
  }

  /**
   * Load model from file if it exists.
   */
  def loadModel(path: String): Option[DJRLModelState[Board, Move]] = {
    val modelPath = Paths.get(path)
    val paramFile = Paths.get(path, "q-network-0000.params")
    if (Files.exists(paramFile)) {
      println(s"Loading model from: $path")
      val model = modelControl.initialModel
      model.qNetwork.load(modelPath, "q-network")
      // Also copy to target network
      val sourceParams = model.qNetwork.getBlock.getParameters
      val targetParams = model.targetNetwork.getBlock.getParameters
      sourceParams.forEach { entry =>
        val name = entry.getKey
        val sourceParam = entry.getValue
        val targetParam = targetParams.get(name)
        if (targetParam != null && sourceParam.getArray != null) {
          targetParam.getArray.set(sourceParam.getArray.toFloatArray)
        }
      }
      println("Model loaded successfully")
      Some(model)
    } else {
      println(s"No saved model found at: $path")
      None
    }
  }

  def train(): TrainingMetrics = {
    var model = config.modelPath.flatMap(loadModel).getOrElse(modelControl.initialModel)
    var player1Wins = 0
    var player2Wins = 0
    var draws = 0
    var totalGameLength = 0
    var lastBatchTime = System.currentTimeMillis()
    var lastTrainingSteps = 0

    for (episode <- 1 to config.numEpisodes) {
      var state = game.initState
      var stepCount = 0
      var gameOver = false

      val agent = new TikTakToeMiniMaxAgent[LogicF](
        modelControl,
        config.boardSize,
        config.maxRecursionDepth,
        config.trainingMode
      )

      var currentModel = model

      while (!gameOver && !game.isFinalState(state)) {
        val stepResult = agent.performStep(game, state, currentModel, AgentRunningMode.Explore)

        stepResult.fsplit match {
          case Some((scala.util.Success(result), _)) =>
            result match {
              case RLAgentStepResult.Continued(newState, newModel) =>
                state = newState
                currentModel = newModel
                stepCount += 1

              case RLAgentStepResult.Finished(finalState, newModel) =>
                state = finalState
                currentModel = newModel
                stepCount += 1
                gameOver = true

              case RLAgentStepResult.InvalidAction(newModel) =>
                currentModel = newModel
                stepCount += 1
            }

          case Some((scala.util.Failure(e), _)) =>
            println(s"Error during step: ${e.getMessage}")
            gameOver = true

          case None =>
            gameOver = true
        }
      }

      model = currentModel

      totalGameLength += stepCount
      state.board.winner match {
        case Some(1) => player1Wins += 1
        case Some(2) => player2Wins += 1
        case _ => draws += 1
      }

      if (episode % 50 == 0) {
        val now = System.currentTimeMillis()
        val batchTime = (now - lastBatchTime) / 1000.0
        lastBatchTime = now
        val trainingSteps = model.totalTrainingSteps - lastTrainingSteps
        lastTrainingSteps = model.totalTrainingSteps
        val avgLen = totalGameLength.toDouble / episode
        println(f"Episode $episode: P1=$player1Wins, P2=$player2Wins, draws=$draws, avg_len=$avgLen%.1f, trains=$trainingSteps, batch_time=${batchTime}%.1fs")
      }

      if (episode % config.saveEvery == 0) {
        config.modelPath.foreach(path => saveModel(model, path))
      }

      // Force GC every 1000 episodes to test if memory is recoverable
      if (episode % 1000 == 0) {
        System.gc()
      }
    }

    // Save final model
    config.modelPath.foreach(path => saveModel(model, path))

    model.close()

    TrainingMetrics(
      episode = config.numEpisodes,
      player1Wins = player1Wins,
      player2Wins = player2Wins,
      draws = draws,
      avgGameLength = totalGameLength.toDouble / config.numEpisodes
    )
  }
}

object MiniMaxTrainer {

  def run(config: MiniMaxConfig = MiniMaxConfig()): TrainingMetrics = {
    val device = SelfPlayTrainer.detectDevice()
    val ndManager = NDManager.newBaseManager(device)
    try {
      given NDManager = ndManager
      val trainer = new MiniMaxTrainer(config)
      trainer.train()
    } finally {
      ndManager.close()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting TikTakToe MiniMax training...")
    val metrics = run()
    println(s"\nTraining complete!")
    println(s"Final metrics: P1 wins=${metrics.player1Wins}, P2 wins=${metrics.player2Wins}, draws=${metrics.draws}")
    println(f"Average game length: ${metrics.avgGameLength}%.1f moves")
  }
}
