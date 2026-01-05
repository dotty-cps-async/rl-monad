package cps.learning.examples.tiktaktoe

import scala.util.{Random, Success, Failure, Try}
import scala.collection.mutable.ArrayBuffer
import java.nio.file.{Path, Paths, Files}
import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.Device
import ai.djl.engine.Engine

import cps.*
import cps.monads.{CpsIdentity, given}
import cps.learning.*
import cps.learning.backends.jql.*
import cps.learning.backends.jql.given  // TensorScope[NDManager]
import cps.learning.ScoredLogicStreamT
import cps.learning.ScoredLogicStreamT.given
import cps.learning.ds.LogicalSearchPolicy.given

case class LogicTreeConfig(
  boardSize: Int = 5,
  winLength: Int = 4,
  numGames: Int = 5000,           // Total number of games (episodes) to play
  maxParallelGames: Int = 5,      // Maximum concurrent games
  topN: Int = 3,                  // Take top N actions when slots available
  epsilon: Float = 0.2f,
  gamma: Float = 0.99f,
  batchSize: Int = 32,
  nStepsBetweenTraining: Int = 5,
  targetUpdateFrequency: Int = 50,
  saveEvery: Int = 500,           // Save model every N games
  modelPath: Option[String] = Some("models/tiktaktoe-logic-tree"),
  random: Random = new Random()
)

/**
 * An active game being played
 */
case class ActiveGame(
  state: GameState,
  model: DJRLModelState[Board, Move],
  stepCount: Int
)

/**
 * Trainer that explores multiple game trees using a slot-based approach.
 * - When slots are available: take top N actions, spawn N-1 new games
 * - When no slots available: take only the first (best) action
 */
class LogicTreeTrainer(config: LogicTreeConfig)(using TensorScope[NDManager]) {

  type LogicF[A] = ScoredLogicStreamT[CpsIdentity, A, Float]
  type StepResult = RLAgentStepResult[GameState, DJRLModelState[Board, Move]]

  val game = new TikTakToeGame(config.boardSize, config.winLength)

  given moveRepr: IntRepresentation[Move] = MoveIntRepresentation(config.boardSize)
  given boardRepr: BatchableTensorRepresentation[Board, NDManager] { type Tensor = NDArray } =
    BoardTensorRepresentation(config.boardSize)

  val modelParams = DJLRLModelParams(
    name = "tiktaktoe-logic-tree",
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

  class TreeAgentBehavior(mc: DJLRLModelControl[LogicF, GameState, Board, Move])
      extends RLModelAgentBehavior[LogicF, DJRLModelState[Board, Move], GameState, Board, Move, Float](mc) {

    override def possibleActions(env: RLEnvironment[GameState, Board, Move], envState: GameState, agentState: AgentBehaviorState): IndexedSeq[Move] = {
      (for {
        i <- 0 until config.boardSize
        j <- 0 until config.boardSize
        if envState.board.isEmpty(i, j)
      } yield Move(i, j, envState.nextPlayer)).toIndexedSeq
    }
  }

  val agentBehavior = new TreeAgentBehavior(modelControl)

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
      val manager = model.qNetworkTrainer.getManager
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

  /**
   * Take top N elements from a logic stream.
   */
  def takeN[A](stream: LogicF[A], n: Int): List[Try[A]] = {
    var remaining = stream
    var results = List.empty[Try[A]]
    var count = 0

    while (count < n) {
      remaining.fsplit match {
        case Some((result, rest)) =>
          results = result :: results
          remaining = rest
          count += 1
        case None =>
          count = n
      }
    }

    results.reverse
  }

  def train(): TrainingMetrics = {
    // Try to load existing model or create new one
    var model = config.modelPath.flatMap(loadModel).getOrElse(modelControl.initialModel)
    var p1Wins = 0
    var p2Wins = 0
    var draws = 0
    var gamesCompleted = 0

    // Active games being played
    val activeGames = ArrayBuffer[ActiveGame]()

    // Start initial games up to maxParallelGames
    val initialSlots = math.min(config.maxParallelGames, config.numGames)
    for (_ <- 0 until initialSlots) {
      activeGames += ActiveGame(game.initState, model, 0)
    }

    while (gamesCompleted < config.numGames && activeGames.nonEmpty) {
      val newActiveGames = ArrayBuffer[ActiveGame]()
      val availableSlots = config.maxParallelGames - activeGames.size

      for (activeGame <- activeGames) {
        if (game.isFinalState(activeGame.state)) {
          // Game (episode) finished
          activeGame.state.board.winner match {
            case Some(1) => p1Wins += 1
            case Some(2) => p2Wins += 1
            case _ => draws += 1
          }
          model = activeGame.model
          gamesCompleted += 1

          // Start a new game if we haven't reached the limit
          if (gamesCompleted + newActiveGames.size < config.numGames && newActiveGames.size < config.maxParallelGames) {
            newActiveGames += ActiveGame(game.initState, model, 0)
          }

          // Progress and save
          if (gamesCompleted % 100 == 0) {
            println(f"Games: $gamesCompleted/${ config.numGames}, P1=$p1Wins, P2=$p2Wins, draws=$draws, batchTrain=${model.totalTrainingSteps}")
          }
          if (gamesCompleted % config.saveEvery == 0) {
            config.modelPath.foreach(path => saveModel(model, path))
          }
        } else {
          // Get step stream and advance game
          val stepStream = agentBehavior.performStep(game, activeGame.state, activeGame.model, AgentRunningMode.Explore)

          // Take top N if we have slots for new games, otherwise just take 1
          val slotsForNewGames = config.maxParallelGames - newActiveGames.size - 1
          val canSpawn = slotsForNewGames > 0 && (gamesCompleted + newActiveGames.size + activeGames.size) < config.numGames
          val n = if (canSpawn) math.min(config.topN, slotsForNewGames + 1) else 1
          val results = takeN(stepStream, n)

          for ((result, idx) <- results.zipWithIndex) {
            result match {
              case Success(RLAgentStepResult.Continued(newState, newModel)) =>
                if (idx == 0) {
                  newActiveGames += ActiveGame(newState, newModel, activeGame.stepCount + 1)
                } else if (canSpawn && newActiveGames.size < config.maxParallelGames) {
                  newActiveGames += ActiveGame(newState, newModel, activeGame.stepCount + 1)
                }
                model = newModel

              case Success(RLAgentStepResult.Finished(finalState, newModel)) =>
                finalState.board.winner match {
                  case Some(1) => p1Wins += 1
                  case Some(2) => p2Wins += 1
                  case _ => draws += 1
                }
                model = newModel
                gamesCompleted += 1

                if (gamesCompleted % 100 == 0) {
                  println(f"Games: $gamesCompleted/${config.numGames}, P1=$p1Wins, P2=$p2Wins, draws=$draws, batchTrain=${model.totalTrainingSteps}")
                }
                if (gamesCompleted % config.saveEvery == 0) {
                  config.modelPath.foreach(path => saveModel(model, path))
                }

                // Start new game if needed (only for idx == 0 to avoid over-spawning from branching)
                if (idx == 0 && gamesCompleted + newActiveGames.size < config.numGames && newActiveGames.size < config.maxParallelGames) {
                  newActiveGames += ActiveGame(game.initState, model, 0)
                }

              case Success(RLAgentStepResult.InvalidAction(newModel)) =>
                model = newModel
                // Keep the game going with current state if this was the primary branch
                if (idx == 0) {
                  newActiveGames += ActiveGame(activeGame.state, newModel, activeGame.stepCount + 1)
                }

              case Failure(e) =>
                // Keep the game going with current state if this was the primary branch
                if (idx == 0) {
                  newActiveGames += ActiveGame(activeGame.state, activeGame.model, activeGame.stepCount + 1)
                }
            }
          }
        }
      }

      // Safety check: if we still have games to play but newActiveGames is empty, spawn new games
      if (newActiveGames.isEmpty && gamesCompleted < config.numGames) {
        val toSpawn = math.min(config.maxParallelGames, config.numGames - gamesCompleted)
        for (_ <- 0 until toSpawn) {
          newActiveGames += ActiveGame(game.initState, model, 0)
        }
      }

      activeGames.clear()
      activeGames ++= newActiveGames
    }

    // Save final model
    config.modelPath.foreach(path => saveModel(model, path))

    println(s"Total batch training calls: ${model.totalTrainingSteps}")

    // Close model resources
    model.close()

    TrainingMetrics(
      episode = gamesCompleted,
      player1Wins = p1Wins,
      player2Wins = p2Wins,
      draws = draws,
      avgGameLength = 0.0 // TODO: track actual steps
    )
  }
}

object LogicTreeTrainer {

  def detectDevice(): Device = {
    val engine = Engine.getInstance()
    val gpuCount = engine.getGpuCount()
    if (gpuCount > 0) {
      println(s"GPU detected: $gpuCount GPU(s) available, using GPU 0")
      Device.gpu(0)
    } else {
      println("No GPU detected, using CPU")
      Device.cpu()
    }
  }

  def run(config: LogicTreeConfig = LogicTreeConfig()): TrainingMetrics = {
    val device = detectDevice()
    given TensorPlatform { type Scope = NDManager } = DJL.withDevice(device)
    TensorScope.withGlobalScope[NDManager, TrainingMetrics] { rootScope =>
      val trainer = new LogicTreeTrainer(config)
      trainer.train()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting TikTakToe Logic Tree Training...")
    val config = LogicTreeConfig()
    println(s"Total games: ${config.numGames}, Max parallel: ${config.maxParallelGames}, Top N: ${config.topN}")
    val metrics = run(config)
    println(s"\nTraining complete!")
    println(s"Final: P1=${metrics.player1Wins}, P2=${metrics.player2Wins}, draws=${metrics.draws}")
    println(s"Total games played: ${metrics.episode}")
  }
}
