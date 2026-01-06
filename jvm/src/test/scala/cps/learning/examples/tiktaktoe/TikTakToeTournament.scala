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
import cps.learning.backends.jql.given
import cps.learning.tournament.*
import cps.learning.ScoredLogicStreamT
import cps.learning.ScoredLogicStreamT.given
import cps.learning.ds.LogicalSearchPolicy.given

/**
 * Model architecture type for TikTakToe agents.
 */
enum TikTakToeModelType {
  case MLP    // Multi-layer perceptron (flat input)
  case CNN    // Convolutional neural network (2D input)
}

/**
 * Descriptor for a TikTakToe tournament agent.
 *
 * @param name Display name for the agent
 * @param modelPath Path to saved model parameters
 * @param modelType Architecture type (MLP or CNN)
 * @param boardSize Board size (default 5)
 * @param winLength Consecutive pieces to win (default 4)
 * @param maxRecursionDepth MiniMax search depth (default 4)
 * @param epsilon Exploration rate for training mode (default 0)
 */
case class TikTakToeAgent(
  name: String,
  modelPath: String,
  modelType: TikTakToeModelType,
  boardSize: Int = 5,
  winLength: Int = 4,
  maxRecursionDepth: Int = 4,
  epsilon: Float = 0.0f  // Default 0 for evaluation (deterministic)
)

/**
 * Type alias for the logic monad used in tournament play.
 */
type TournamentLogicF[A] = ScoredLogicStreamT[CpsIdentity, A, Float]

/**
 * Loaded agent with all resources needed to play games.
 */
private class LoadedAgent(
  val descriptor: TikTakToeAgent,
  val modelState: DJRLModelState[GameState, Move],
  val modelControl: DJLRLModelControl[TournamentLogicF, GameState, GameState, Move]
) {
  def close(): Unit = {
    modelState.close()
  }
}

/**
 * TikTakToe-specific match runner implementation.
 */
class TikTakToeTournamentMatchRunner(using scope: TensorScope[NDManager]) extends TournamentMatchRunner[TikTakToeAgent] {

  override def agentName(agent: TikTakToeAgent): String = agent.name

  override def playMatch(
    agent1: TikTakToeAgent,
    agent2: TikTakToeAgent,
    games: Int,
    mode: MatchMode,
    alternateFirst: Boolean
  ): MatchResult = {
    // Load both agents
    val loaded1 = loadAgent(agent1, mode)
    val loaded2 = loadAgent(agent2, mode)

    try {
      var agent1Wins = 0
      var agent2Wins = 0
      var draws = 0

      for (gameIdx <- 0 until games) {
        // Alternate first player if requested
        val (firstAgent, secondAgent, swapped) =
          if (alternateFirst && gameIdx % 2 == 1) (loaded2, loaded1, true)
          else (loaded1, loaded2, false)

        val result = playGame(firstAgent, secondAgent, mode)

        result match {
          case GameOutcome.Player1Wins =>
            if (swapped) agent2Wins += 1 else agent1Wins += 1
          case GameOutcome.Player2Wins =>
            if (swapped) agent1Wins += 1 else agent2Wins += 1
          case GameOutcome.Draw =>
            draws += 1
        }
      }

      MatchResult(agent1.name, agent2.name, agent1Wins, agent2Wins, draws)
    } finally {
      loaded1.close()
      loaded2.close()
    }
  }

  private enum GameOutcome {
    case Player1Wins, Player2Wins, Draw
  }

  /**
   * Play a single game between two agents.
   * Player 1 goes first.
   */
  private def playGame(player1: LoadedAgent, player2: LoadedAgent, mode: MatchMode): GameOutcome = {
    val game = new TikTakToeGame(player1.descriptor.boardSize, player1.descriptor.winLength)
    var state = game.initState

    val agent1 = createAgentBehavior(player1, mode)
    val agent2 = createAgentBehavior(player2, mode)

    var model1 = player1.modelState
    var model2 = player2.modelState

    val runningMode = mode match {
      case MatchMode.Evaluate => AgentRunningMode.Exploit
      case MatchMode.Train => AgentRunningMode.Explore
    }

    while (!game.isFinalState(state)) {
      val (currentAgent, currentModel, isPlayer1) =
        if (state.nextPlayer == 1) (agent1, model1, true)
        else (agent2, model2, false)

      val stepResult = currentAgent.performStep(game, state, currentModel, runningMode)

      stepResult.fsplit match {
        case Some((scala.util.Success(stepped), _)) =>
          stepped match {
            case RLAgentStepResult.Continued(newState, newModel) =>
              state = newState
              if (isPlayer1) model1 = newModel else model2 = newModel

            case RLAgentStepResult.Finished(finalState, newModel) =>
              state = finalState
              if (isPlayer1) model1 = newModel else model2 = newModel

            case RLAgentStepResult.InvalidAction(newModel) =>
              // Invalid action - game ends with loss for current player
              if (isPlayer1) model1 = newModel else model2 = newModel
              return if (isPlayer1) GameOutcome.Player2Wins else GameOutcome.Player1Wins
          }
        case Some((scala.util.Failure(e), _)) =>
          // Error - treat as draw
          return GameOutcome.Draw
        case None =>
          // No result - treat as draw
          return GameOutcome.Draw
      }
    }

    // Game ended normally
    state.board.winner match {
      case Some(1) => GameOutcome.Player1Wins
      case Some(2) => GameOutcome.Player2Wins
      case _ => GameOutcome.Draw
    }
  }

  private def createAgentBehavior(
    agent: LoadedAgent,
    mode: MatchMode
  ): TikTakToeMiniMaxAgentForTournament[TournamentLogicF] = {
    // Always use OnCommit - the runningMode controls exploration vs exploitation
    new TikTakToeMiniMaxAgentForTournament[TournamentLogicF](
      agent.modelControl,
      agent.descriptor.boardSize,
      agent.descriptor.maxRecursionDepth,
      VirtualStepTrainingMode.OnCommit
    )
  }

  private def loadAgent(descriptor: TikTakToeAgent, mode: MatchMode): LoadedAgent = {
    val epsilon = mode match {
      case MatchMode.Evaluate => 0.0f  // No exploration during evaluation
      case MatchMode.Train => descriptor.epsilon
    }

    descriptor.modelType match {
      case TikTakToeModelType.MLP =>
        loadMLPAgent(descriptor, epsilon)
      case TikTakToeModelType.CNN =>
        loadCNNAgent(descriptor, epsilon)
    }
  }

  private def loadMLPAgent(descriptor: TikTakToeAgent, epsilon: Float): LoadedAgent = {
    given moveRepr: IntRepresentation[Move] = MoveIntRepresentation(descriptor.boardSize)
    given gameStateRepr: BatchableTensorRepresentation[GameState, NDManager] { type Tensor = ai.djl.ndarray.NDArray } =
      GameStateTensorRepresentation(descriptor.boardSize)

    val modelParams = DJLRLModelParams(
      name = s"${descriptor.name}-q",
      qBuilder = () => DQNBoardModel.buildBlock(
        2 * descriptor.boardSize * descriptor.boardSize,
        descriptor.boardSize * descriptor.boardSize
      ),
      observationSize = 2 * descriptor.boardSize * descriptor.boardSize,
      actionSize = descriptor.boardSize * descriptor.boardSize,
      epsilon = epsilon,
      gamma = 0.99f,
      minBatchSize = 32,
      nStepsBetweenTraining = 5,
      targetUpdateFrequency = 50,
      random = new Random()
    )

    val modelControl = new DJLRLModelControl[TournamentLogicF, GameState, GameState, Move](modelParams)
    val modelState = loadModelState(modelControl, descriptor.modelPath)
    new LoadedAgent(descriptor, modelState, modelControl)
  }

  private def loadCNNAgent(descriptor: TikTakToeAgent, epsilon: Float): LoadedAgent = {
    given moveRepr: IntRepresentation[Move] = MoveIntRepresentation(descriptor.boardSize)
    given gameStateRepr: BatchableTensorRepresentation[GameState, NDManager] { type Tensor = ai.djl.ndarray.NDArray } =
      GameStateCNNTensorRepresentation(descriptor.boardSize)

    val modelParams = DJLRLModelParams(
      name = s"${descriptor.name}-q",
      qBuilder = () => DQNBoardModel.buildCNNBlock(descriptor.boardSize),
      observationSize = 2 * descriptor.boardSize * descriptor.boardSize,
      actionSize = descriptor.boardSize * descriptor.boardSize,
      epsilon = epsilon,
      gamma = 0.99f,
      minBatchSize = 32,
      nStepsBetweenTraining = 5,
      targetUpdateFrequency = 50,
      random = new Random(),
      inputShape = Some(Seq(2, descriptor.boardSize, descriptor.boardSize))
    )

    val modelControl = new DJLRLModelControl[TournamentLogicF, GameState, GameState, Move](modelParams)
    val modelState = loadModelState(modelControl, descriptor.modelPath)
    new LoadedAgent(descriptor, modelState, modelControl)
  }

  private def loadModelState(
    modelControl: DJLRLModelControl[TournamentLogicF, GameState, GameState, Move],
    path: String
  ): DJRLModelState[GameState, Move] = {
    val modelPath = Paths.get(path)
    val paramFile = Paths.get(path, "q-network-0000.params")

    val model = modelControl.initialModel

    if (Files.exists(paramFile)) {
      model.qNetwork.load(modelPath, "q-network")
      // Copy to target network
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
    } else {
      println(s"Warning: No saved model found at $path, using random initialization")
    }

    model
  }
}

/**
 * MiniMax agent for tournament play.
 */
private class TikTakToeMiniMaxAgentForTournament[F[_]: CpsScoredLogicMonad.Curry[Float]](
  modelControl: RLModelControl[F, GameState, GameState, Move, Float, DJRLModelState[GameState, Move]],
  boardSize: Int,
  maxDepth: Int,
  trainingMode: VirtualStepTrainingMode
) extends RLMiniMaxAgentBehavior[F, GameState, GameState, Move, DJRLModelState[GameState, Move], Float](
  modelControl, maxDepth, trainingMode
) {
  private val ordering = summon[LinearlyOrderedGroup[Float]]

  override def possibleActions(
    env: RLEnvironment[GameState, GameState, Move],
    state: GameState,
    agentState: DJRLModelState[GameState, Move]
  ): IndexedSeq[Move] = {
    (for {
      i <- 0 until boardSize
      j <- 0 until boardSize
      if state.board.isEmpty(i, j)
    } yield Move(i, j, state.nextPlayer)).toIndexedSeq
  }

  override def gainEstimation(
    env: RLEnvironment[GameState, GameState, Move],
    state: GameState,
    reward: Float
  ): Float = {
    if reward > 0.5f then ordering.maxPositiveValue
    else if reward < -0.5f then 0.0f
    else if env.isFinalState(state) then 0.5f
    else ordering.one
  }

  override def lossThreshold: Float = 0.1f
}

/**
 * Registry of available TikTakToe agents.
 */
object TikTakToeAgentRegistry {
  /**
   * Default agents - requires trained models in models/ directory.
   */
  val defaultAgents: List[TikTakToeAgent] = List(
    TikTakToeAgent(
      name = "MLP-Baseline",
      modelPath = "models/tiktaktoe-minimax-baseline",
      modelType = TikTakToeModelType.MLP
    ),
    TikTakToeAgent(
      name = "CNN-v1",
      modelPath = "models/tiktaktoe-minimax-cnn",
      modelType = TikTakToeModelType.CNN
    )
  )
}

/**
 * TikTakToe tournament runner entry point.
 */
object TikTakToeTournamentRunner {

  def run(
    agents: List[TikTakToeAgent] = TikTakToeAgentRegistry.defaultAgents,
    gamesPerMatch: Int = 20,
    mode: MatchMode = MatchMode.Evaluate
  ): TournamentResult = {
    val device = SelfPlayTrainer.detectDevice()
    given TensorPlatform { type Scope = NDManager } = DJL.withDevice(device)

    TensorScope.withGlobalScope[NDManager, TournamentResult] { rootScope =>
      given matchRunner: TournamentMatchRunner[TikTakToeAgent] = new TikTakToeTournamentMatchRunner()

      val config = TournamentConfig(
        agents = agents,
        gamesPerMatch = gamesPerMatch,
        matchMode = mode,
        alternateFirstPlayer = true
      )

      val tournament = new Tournament(config)
      tournament.run()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting TikTakToe Tournament...")
    println()

    val result = run()
    result.printStandings()
    println()
    result.printMatchResults()
  }
}

/**
 * Cross-training runner: two models learn by playing against each other.
 * Both models are updated during games and saved afterward.
 */
object CrossTrainingRunner {

  case class CrossTrainingConfig(
    agent1: TikTakToeAgent,
    agent2: TikTakToeAgent,
    numGames: Int = 1000,
    epsilon: Float = 0.2f,
    reportEvery: Int = 50,
    saveEvery: Int = 200
  )

  case class CrossTrainingResult(
    agent1Wins: Int,
    agent2Wins: Int,
    draws: Int
  ) {
    def total: Int = agent1Wins + agent2Wins + draws
    def printSummary(agent1Name: String, agent2Name: String): Unit = {
      println(s"\n=== Cross-Training Complete ===")
      println(s"Total games: $total")
      println(s"$agent1Name wins: $agent1Wins (${agent1Wins * 100 / total}%)")
      println(s"$agent2Name wins: $agent2Wins (${agent2Wins * 100 / total}%)")
      println(s"Draws: $draws (${draws * 100 / total}%)")
    }
  }

  def run(config: CrossTrainingConfig): CrossTrainingResult = {
    val device = SelfPlayTrainer.detectDevice()
    given TensorPlatform { type Scope = NDManager } = DJL.withDevice(device)

    TensorScope.withGlobalScope[NDManager, CrossTrainingResult] { rootScope =>
      val trainer = new CrossTrainer(config)
      trainer.train()
    }
  }

  def main(args: Array[String]): Unit = {
    println("Starting Cross-Training: MLP-Baseline vs CNN-v1")
    println("Both models will learn by playing against each other.\n")

    val config = CrossTrainingConfig(
      agent1 = TikTakToeAgent(
        name = "MLP-Baseline",
        modelPath = "models/tiktaktoe-minimax-baseline",
        modelType = TikTakToeModelType.MLP,
        epsilon = 0.2f
      ),
      agent2 = TikTakToeAgent(
        name = "CNN-v1",
        modelPath = "models/tiktaktoe-minimax-cnn",
        modelType = TikTakToeModelType.CNN,
        epsilon = 0.2f
      ),
      numGames = 1000
    )

    val result = run(config)
    result.printSummary(config.agent1.name, config.agent2.name)
  }
}

/**
 * Cross-trainer that handles the actual training loop.
 */
private class CrossTrainer(config: CrossTrainingRunner.CrossTrainingConfig)(using TensorScope[NDManager]) {

  private enum GameOutcome {
    case Agent1Wins, Agent2Wins, Draw
  }

  def train(): CrossTrainingRunner.CrossTrainingResult = {
    // Load both models with exploration enabled
    val (model1, control1, save1) = loadModel(config.agent1, config.epsilon)
    val (model2, control2, save2) = loadModel(config.agent2, config.epsilon)

    var currentModel1 = model1
    var currentModel2 = model2

    var agent1Wins = 0
    var agent2Wins = 0
    var draws = 0

    val game = new TikTakToeGame(config.agent1.boardSize, config.agent1.winLength)
    val startTime = System.currentTimeMillis()
    var lastReportTime = startTime

    for (gameIdx <- 1 to config.numGames) {
      // Alternate who goes first
      val agent1First = gameIdx % 2 == 1

      val (outcome, newModel1, newModel2) = playTrainingGame(
        game, control1, control2, currentModel1, currentModel2, agent1First
      )

      currentModel1 = newModel1
      currentModel2 = newModel2

      outcome match {
        case GameOutcome.Agent1Wins => agent1Wins += 1
        case GameOutcome.Agent2Wins => agent2Wins += 1
        case GameOutcome.Draw => draws += 1
      }

      // Report progress
      if (gameIdx % config.reportEvery == 0) {
        val now = System.currentTimeMillis()
        val elapsed = (now - lastReportTime) / 1000.0
        lastReportTime = now
        val totalElapsed = (now - startTime) / 1000.0
        println(f"Game $gameIdx/${config.numGames}: ${config.agent1.name}=$agent1Wins, ${config.agent2.name}=$agent2Wins, draws=$draws (batch: ${elapsed}%.1fs, total: ${totalElapsed}%.0fs)")
      }

      // Save periodically
      if (gameIdx % config.saveEvery == 0) {
        save1(currentModel1)
        save2(currentModel2)
        println(s"  Models saved at game $gameIdx")
      }

      // Periodic GC
      if (gameIdx % 500 == 0) {
        System.gc()
      }
    }

    // Final save
    save1(currentModel1)
    save2(currentModel2)
    println(s"Final models saved.")

    // Cleanup
    currentModel1.close()
    currentModel2.close()

    CrossTrainingRunner.CrossTrainingResult(agent1Wins, agent2Wins, draws)
  }

  private def playTrainingGame(
    game: TikTakToeGame,
    control1: DJLRLModelControl[TournamentLogicF, GameState, GameState, Move],
    control2: DJLRLModelControl[TournamentLogicF, GameState, GameState, Move],
    model1: DJRLModelState[GameState, Move],
    model2: DJRLModelState[GameState, Move],
    agent1First: Boolean
  ): (GameOutcome, DJRLModelState[GameState, Move], DJRLModelState[GameState, Move]) = {

    var state = game.initState
    var currentModel1 = model1
    var currentModel2 = model2

    val agent1 = new TikTakToeMiniMaxAgentForTournament[TournamentLogicF](
      control1, config.agent1.boardSize, config.agent1.maxRecursionDepth, VirtualStepTrainingMode.OnCommit
    )
    val agent2 = new TikTakToeMiniMaxAgentForTournament[TournamentLogicF](
      control2, config.agent2.boardSize, config.agent2.maxRecursionDepth, VirtualStepTrainingMode.OnCommit
    )

    // Map player number to agent
    val (player1Agent, player2Agent) = if (agent1First) (agent1, agent2) else (agent2, agent1)
    val (player1Control, player2Control) = if (agent1First) (control1, control2) else (control2, control1)

    while (!game.isFinalState(state)) {
      val isPlayer1Turn = state.nextPlayer == 1
      // Select agent based on whose turn it is
      val currentAgent = if (isPlayer1Turn) player1Agent else player2Agent
      // Determine if this is agent1 (MLP) or agent2 (CNN) to select correct model
      val isAgent1 = (currentAgent eq agent1)

      val currentModel = if (isAgent1) currentModel1 else currentModel2

      val stepResult = currentAgent.performStep(game, state, currentModel, AgentRunningMode.Explore)

      stepResult.fsplit match {
        case Some((scala.util.Success(stepped), _)) =>
          stepped match {
            case RLAgentStepResult.Continued(newState, newModel) =>
              state = newState
              if (isAgent1) currentModel1 = newModel else currentModel2 = newModel

            case RLAgentStepResult.Finished(finalState, newModel) =>
              state = finalState
              if (isAgent1) currentModel1 = newModel else currentModel2 = newModel

            case RLAgentStepResult.InvalidAction(newModel) =>
              if (isAgent1) currentModel1 = newModel else currentModel2 = newModel
              // Invalid action - loss for current agent
              val outcome = if (isAgent1) GameOutcome.Agent2Wins else GameOutcome.Agent1Wins
              return (outcome, currentModel1, currentModel2)
          }
        case _ =>
          return (GameOutcome.Draw, currentModel1, currentModel2)
      }
    }

    // Determine winner - check which agent was player1/player2
    val outcome = state.board.winner match {
      case Some(1) =>
        // Player 1 won - is player1Agent agent1 or agent2?
        if (player1Agent eq agent1) GameOutcome.Agent1Wins else GameOutcome.Agent2Wins
      case Some(2) =>
        // Player 2 won
        if (player2Agent eq agent1) GameOutcome.Agent1Wins else GameOutcome.Agent2Wins
      case _ =>
        GameOutcome.Draw
    }

    (outcome, currentModel1, currentModel2)
  }

  private def loadModel(
    descriptor: TikTakToeAgent,
    epsilon: Float
  ): (DJRLModelState[GameState, Move], DJLRLModelControl[TournamentLogicF, GameState, GameState, Move], DJRLModelState[GameState, Move] => Unit) = {
    // Use separate methods to ensure proper given resolution
    // (match branches share scope, causing wrong representation to be captured)
    descriptor.modelType match {
      case TikTakToeModelType.MLP => loadMLPModel(descriptor, epsilon)
      case TikTakToeModelType.CNN => loadCNNModel(descriptor, epsilon)
    }
  }

  private def loadMLPModel(
    descriptor: TikTakToeAgent,
    epsilon: Float
  ): (DJRLModelState[GameState, Move], DJLRLModelControl[TournamentLogicF, GameState, GameState, Move], DJRLModelState[GameState, Move] => Unit) = {
    given IntRepresentation[Move] = MoveIntRepresentation(descriptor.boardSize)
    given BatchableTensorRepresentation[GameState, NDManager] { type Tensor = ai.djl.ndarray.NDArray } =
      GameStateTensorRepresentation(descriptor.boardSize)

    val modelParams = DJLRLModelParams(
      name = s"${descriptor.name}-q",
      qBuilder = () => DQNBoardModel.buildBlock(
        2 * descriptor.boardSize * descriptor.boardSize,
        descriptor.boardSize * descriptor.boardSize
      ),
      observationSize = 2 * descriptor.boardSize * descriptor.boardSize,
      actionSize = descriptor.boardSize * descriptor.boardSize,
      epsilon = epsilon,
      gamma = 0.99f,
      minBatchSize = 32,
      nStepsBetweenTraining = 5,
      targetUpdateFrequency = 50,
      random = new Random()
    )

    val modelControl = new DJLRLModelControl[TournamentLogicF, GameState, GameState, Move](modelParams)
    val modelState = loadModelState(modelControl, descriptor.modelPath)
    val saveFunc = (m: DJRLModelState[GameState, Move]) => saveModel(m, descriptor.modelPath)
    (modelState, modelControl, saveFunc)
  }

  private def loadCNNModel(
    descriptor: TikTakToeAgent,
    epsilon: Float
  ): (DJRLModelState[GameState, Move], DJLRLModelControl[TournamentLogicF, GameState, GameState, Move], DJRLModelState[GameState, Move] => Unit) = {
    given IntRepresentation[Move] = MoveIntRepresentation(descriptor.boardSize)
    given BatchableTensorRepresentation[GameState, NDManager] { type Tensor = ai.djl.ndarray.NDArray } =
      GameStateCNNTensorRepresentation(descriptor.boardSize)

    val modelParams = DJLRLModelParams(
      name = s"${descriptor.name}-q",
      qBuilder = () => DQNBoardModel.buildCNNBlock(descriptor.boardSize),
      observationSize = 2 * descriptor.boardSize * descriptor.boardSize,
      actionSize = descriptor.boardSize * descriptor.boardSize,
      epsilon = epsilon,
      gamma = 0.99f,
      minBatchSize = 32,
      nStepsBetweenTraining = 5,
      targetUpdateFrequency = 50,
      random = new Random(),
      inputShape = Some(Seq(2, descriptor.boardSize, descriptor.boardSize))
    )

    val modelControl = new DJLRLModelControl[TournamentLogicF, GameState, GameState, Move](modelParams)
    val modelState = loadModelState(modelControl, descriptor.modelPath)
    val saveFunc = (m: DJRLModelState[GameState, Move]) => saveModel(m, descriptor.modelPath)
    (modelState, modelControl, saveFunc)
  }

  private def loadModelState(
    modelControl: DJLRLModelControl[TournamentLogicF, GameState, GameState, Move],
    path: String
  ): DJRLModelState[GameState, Move] = {
    val modelPath = Paths.get(path)
    val paramFile = Paths.get(path, "q-network-0000.params")

    val model = modelControl.initialModel

    if (Files.exists(paramFile)) {
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
      println(s"Loaded model from: $path")
    } else {
      println(s"Warning: No saved model found at $path, using random initialization")
    }

    model
  }

  private def saveModel(model: DJRLModelState[GameState, Move], path: String): Unit = {
    val dir = Paths.get(path).getParent
    if (dir != null && !Files.exists(dir)) {
      Files.createDirectories(dir)
    }
    model.qNetwork.save(Paths.get(path), "q-network")
  }
}
