package cps.learning.examples.tiktaktoe

import munit.FunSuite
import scala.util.Random

class TikTakToeTrainingTest extends FunSuite {

  test("SelfPlayTrainer should run training without errors") {
    val config = SelfPlayConfig(
      boardSize = 3,
      winLength = 3,
      numEpisodes = 50,
      epsilon = 0.5f,
      epsilonDecay = 0.99f,
      minEpsilon = 0.1f,
      batchSize = 16,
      replayBufferSize = 1000,
      nStepsBetweenTraining = 5,
      targetUpdateFrequency = 20,
      random = new Random(42)
    )

    try {
      val metrics = SelfPlayTrainer.run(config)

      // Basic sanity checks
      assertEquals(metrics.episode, 50)
      assert(metrics.player1Wins + metrics.player2Wins + metrics.draws == 50)
      assert(metrics.avgGameLength > 0)
    } catch {
      case e: ai.djl.engine.EngineException if e.getMessage.contains("No PyTorch native library") =>
        // Skip test if PyTorch is not available on this platform
        assume(false, s"PyTorch not available: ${e.getMessage}")
    }
  }

  test("MiniMaxTrainer should run training without errors") {
    val config = MiniMaxConfig(
      boardSize = 3,
      winLength = 3,
      maxRecursionDepth = 2,
      numEpisodes = 20,
      epsilon = 0.5f,
      batchSize = 16,
      nStepsBetweenTraining = 5,
      targetUpdateFrequency = 10,
      modelPath = None,
      random = new Random(42)
    )

    try {
      val metrics = MiniMaxTrainer.run(config)

      // Basic sanity checks
      assertEquals(metrics.episode, 20)
      assert(metrics.player1Wins + metrics.player2Wins + metrics.draws == 20)
      assert(metrics.avgGameLength > 0)
    } catch {
      case e: ai.djl.engine.EngineException if e.getMessage.contains("No PyTorch native library") =>
        assume(false, s"PyTorch not available: ${e.getMessage}")
    }
  }

  test("TikTakToeGame should produce valid game states") {
    val game = new TikTakToeGame(3, 3)
    val initState = game.initState

    assertEquals(initState.nextPlayer, 1)
    assertEquals(initState.board.isFull, false)
    assertEquals(game.isFinalState(initState), false)

    // Test observation
    val observation = game.observe(initState)
    assertEquals(observation.size, 3)
    assertEquals(observation.isFull, false)
  }

  test("TikTakToeGame should detect winner correctly") {
    val game = new TikTakToeGame(3, 3)
    var state = game.initState

    // Player 1 wins with diagonal
    val moves = Seq(
      Move(0, 0, 1), Move(0, 1, 2),
      Move(1, 1, 1), Move(0, 2, 2),
      Move(2, 2, 1)  // Player 1 wins
    )

    for (move <- moves if !game.isFinalState(state)) {
      game.applyAction(state, move) match {
        case Some((newState, _)) => state = newState
        case None => fail(s"Invalid move: $move")
      }
    }

    assert(game.isFinalState(state))
    assertEquals(state.board.winner, Some(1))
  }

  test("MoveIntRepresentation should encode and decode correctly") {
    val repr = MoveIntRepresentation(5)

    // Test encoding
    assertEquals(repr.toTensor(Move(0, 0, 1)), 0)
    assertEquals(repr.toTensor(Move(0, 4, 1)), 4)
    assertEquals(repr.toTensor(Move(1, 0, 1)), 5)
    assertEquals(repr.toTensor(Move(4, 4, 1)), 24)

    // Test decoding
    assertEquals(repr.fromTensor(0), Some(Move(0, 0, 0)))
    assertEquals(repr.fromTensor(5), Some(Move(1, 0, 0)))
    assertEquals(repr.fromTensor(24), Some(Move(4, 4, 0)))

    // Test invalid index
    assertEquals(repr.fromTensor(-1), None)
    assertEquals(repr.fromTensor(25), None)
  }
}
