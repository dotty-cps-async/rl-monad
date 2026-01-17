package cps.rl.examples.tiktaktoe

import scala.util.Random
import java.nio.file.{Path, Paths, Files}
import ai.djl.ndarray.NDManager
import ai.djl.Device
import ai.djl.engine.Engine

import cps.*
import cps.monads.{CpsIdentity, given}
import cps.monads.logic.{*, given}
import cps.rl.*
import cps.rl.backends.jql.*
import cps.rl.backends.jql.given  // TensorScope[NDManager]
import cps.rl.ScoredLogicStreamT
import cps.rl.ScoredLogicStreamT.given
import cps.rl.ds.LogicalSearchPolicy.given

/**
 * Configuration for CNN-based MiniMax trainer.
 * Uses convolutional neural network for spatial pattern recognition.
 */
case class MiniMaxCNNConfig(
  boardSize: Int = 5,
  winLength: Int = 4,
  maxRecursionDepth: Int = 4,
  maxBranches: Int = 5,
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
  modelPath: Option[String] = Some("models/tiktaktoe-minimax-cnn"),
  random: Random = new Random()
)

/**
 * CNN-based MiniMax agent for TikTakToe.
 */
class TikTakToeMiniMaxAgentCNN[F[_]: CpsScoredLogicMonad.Curry[Float]](
  modelControl: RLModelControl[F, GameState, GameState, Move, Float, DJRLModelState[GameState, Move]],
  boardSize: Int,
  maxDepth: Int,
  trainingMode: VirtualStepTrainingMode
) extends RLMiniMaxAgentBehavior[F, GameState, GameState, Move, DJRLModelState[GameState, Move], Float](
  modelControl, maxDepth, trainingMode
) {
  private val ordering = summon[Ordering[Float]]
  private val factorGroup = summon[ScalingGroup[Float]]

  override def possibleActions(env: RLEnvironment[GameState, GameState, Move], state: GameState, agentState: DJRLModelState[GameState, Move]): IndexedSeq[Move] = {
    (for {
      i <- 0 until boardSize
      j <- 0 until boardSize
      if state.board.isEmpty(i, j)
    } yield Move(i, j, state.nextPlayer)).toIndexedSeq
  }

  override def gainEstimation(env: RLEnvironment[GameState, GameState, Move], state: GameState, reward: Float): Float = {
    if reward > 0.5f then factorGroup.maxPositiveValue
    else if reward < -0.5f then 0.0f
    else if env.isFinalState(state) then 0.5f
    else factorGroup.one
  }

  override def lossThreshold: Float = 0.1f
}

/**
 * CNN-based MiniMax trainer for TikTakToe.
 * Uses convolutional neural network to learn spatial patterns on the board.
 */
class MiniMaxTrainerCNN(config: MiniMaxCNNConfig)(using TensorScope[NDManager]) {

  type LogicF[A] = ScoredLogicStreamT[CpsIdentity, A, Float]

  val game = new TikTakToeGame(config.boardSize, config.winLength)

  given moveRepr: IntRepresentation[Move] = MoveIntRepresentation(config.boardSize)

  // Use CNN-compatible tensor representation with shape (2, boardSize, boardSize)
  given gameStateRepr: BatchableTensorRepresentation[GameState, NDManager] { type Tensor = ai.djl.ndarray.NDArray } =
    GameStateCNNTensorRepresentation(config.boardSize)

  // CNN model parameters
  // inputShape specifies the 2D structure: (channels, height, width)
  val modelParams = DJLRLModelParams(
    name = "tiktaktoe-minimax-cnn",
    qBuilder = () => DQNBoardModel.buildCNNBlock(config.boardSize),
    observationSize = 2 * config.boardSize * config.boardSize,  // Total elements for replay buffer
    actionSize = config.boardSize * config.boardSize,
    epsilon = config.epsilon,
    gamma = config.gamma,
    minBatchSize = config.batchSize,
    nStepsBetweenTraining = config.nStepsBetweenTraining,
    targetUpdateFrequency = config.targetUpdateFrequency,
    random = config.random,
    inputShape = Some(Seq(2, config.boardSize, config.boardSize))  // CNN input: (channels, H, W)
  )

  val modelControl = new DJLRLModelControl[LogicF, GameState, GameState, Move](modelParams)

  def saveModel(model: DJRLModelState[GameState, Move], path: String): Unit = {
    val dir = Paths.get(path).getParent
    if (dir != null && !Files.exists(dir)) {
      Files.createDirectories(dir)
    }
    model.qNetwork.save(Paths.get(path), "q-network")
    println(s"Model saved to: $path")
  }

  def loadModel(path: String): Option[DJRLModelState[GameState, Move]] = {
    val modelPath = Paths.get(path)
    val paramFile = Paths.get(path, "q-network-0000.params")
    if (Files.exists(paramFile)) {
      println(s"Loading model from: $path")
      val model = modelControl.initialModel
      model.qNetwork.load(modelPath, "q-network")
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

      val agent = new TikTakToeMiniMaxAgentCNN[LogicF](
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

      if (episode % 1000 == 0) {
        System.gc()
      }
    }

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

object MiniMaxTrainerCNN {

  def run(config: MiniMaxCNNConfig = MiniMaxCNNConfig()): TrainingMetrics = {
    val device = SelfPlayTrainer.detectDevice()
    given TensorPlatform { type Scope = NDManager } = DJL.withDevice(device)
    TensorScope.withGlobalScope[NDManager, TrainingMetrics] { rootScope =>
      val trainer = new MiniMaxTrainerCNN(config)
      trainer.train()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting TikTakToe MiniMax CNN training...")
    val metrics = run()
    println(s"\nTraining complete!")
    println(s"Final metrics: P1 wins=${metrics.player1Wins}, P2 wins=${metrics.player2Wins}, draws=${metrics.draws}")
    println(f"Average game length: ${metrics.avgGameLength}%.1f moves")
  }
}
