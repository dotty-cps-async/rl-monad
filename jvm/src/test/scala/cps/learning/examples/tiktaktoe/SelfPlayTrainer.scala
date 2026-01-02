package cps.learning.examples.tiktaktoe

import scala.util.Random
import ai.djl.ndarray.{NDArray, NDManager}

import cps.*
import cps.monads.{CpsIdentity, given}
import cps.learning.*
import cps.learning.backends.jql.*
import cps.learning.ScoredLogicStreamT
import cps.learning.ScoredLogicStreamT.given
import cps.learning.ds.LogicalSearchPolicy.given

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

/**
 * Self-play trainer using the monadic RL interfaces.
 */
class SelfPlayTrainer(config: SelfPlayConfig)(using ndManager: NDManager) {

  // Use CpsIdentity as the base monad - simplest synchronous execution
  type LogicF[A] = ScoredLogicStreamT[CpsIdentity, A, Float]

  // Game environment
  val game = new TikTakToeGame(config.boardSize, config.winLength)

  // Int representation for Move
  given moveRepr: IntRepresentation[Move] = MoveIntRepresentation(config.boardSize)

  // Model parameters
  val modelParams = DJLRLModelParams(
    name = "tiktaktoe-selfplay",
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

  // Model control using DJL backend
  val modelControl = new DJLRLModelControl[LogicF, GameState, Board, Move](modelParams)

  // Agent behavior that uses the model
  class TikTakToeAgentBehavior(mc: DJLRLModelControl[LogicF, GameState, Board, Move])
      extends RLModelAgentBehavior[LogicF, DJRLModelState[Board, Move], GameState, Board, Move, Float](mc) {

    override def possibleActions(env: RLEnvironment[GameState, Board, Move], envState: GameState, agentState: AgentBehaviorState): IndexedSeq[Move] = {
      (for {
        i <- 0 until config.boardSize
        j <- 0 until config.boardSize
        if envState.board.isEmpty(i, j)
      } yield Move(i, j, envState.nextPlayer)).toIndexedSeq
    }
  }

  val agentBehavior = new TikTakToeAgentBehavior(modelControl)

  def train(): TrainingMetrics = {
    var model = modelControl.initialModel
    var player1Wins = 0
    var player2Wins = 0
    var draws = 0
    var totalGameLength = 0
    var epsilon = config.epsilon

    for (episode <- 1 to config.numEpisodes) {
      var state = game.initState
      var stepCount = 0
      var gameOver = false

      while (!gameOver && !game.isFinalState(state)) {
        // Use agent behavior to perform a step
        val stepResult = agentBehavior.performStep(game, state, model, AgentRunningMode.Explore)

        // Extract first result from the logic stream
        stepResult.fsplit match {
          case Some((scala.util.Success(result), _)) =>
            result match {
              case RLAgentStepResult.Continued(newState, newModel) =>
                state = newState
                model = newModel
                stepCount += 1

              case RLAgentStepResult.Finished(finalState, newModel) =>
                state = finalState
                model = newModel
                stepCount += 1
                gameOver = true

              case RLAgentStepResult.InvalidAction(newModel) =>
                // Shouldn't happen with proper possibleActions, but handle gracefully
                model = newModel
                stepCount += 1
            }

          case Some((scala.util.Failure(e), _)) =>
            // Error in computation
            println(s"Error during step: ${e.getMessage}")
            gameOver = true

          case None =>
            // No result (empty stream) - shouldn't happen
            gameOver = true
        }
      }

      // Record game result
      totalGameLength += stepCount
      state.board.winner match {
        case Some(1) => player1Wins += 1
        case Some(2) => player2Wins += 1
        case _ => draws += 1
      }

      // Decay epsilon (note: this affects future model params, not current model)
      epsilon = math.max(config.minEpsilon, epsilon * config.epsilonDecay).toFloat

      // Log progress periodically
      if (episode % 50 == 0) {
        val avgLen = totalGameLength.toDouble / episode
        println(f"Episode $episode: P1 wins=$player1Wins, P2 wins=$player2Wins, draws=$draws, avg_len=$avgLen%.1f")
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
}

object SelfPlayTrainer {

  def run(config: SelfPlayConfig = SelfPlayConfig()): TrainingMetrics = {
    val ndManager = NDManager.newBaseManager()
    try {
      given NDManager = ndManager
      val trainer = new SelfPlayTrainer(config)
      trainer.train()
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
