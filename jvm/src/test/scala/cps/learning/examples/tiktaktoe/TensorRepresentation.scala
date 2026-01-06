package cps.learning.examples.tiktaktoe

import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.ndarray.types.Shape
import cps.learning.{TensorScope, BatchableTensorRepresentation}

/**
 * Scoped tensor representation for GameState with relative encoding.
 *
 * Encodes the board from the current player's perspective:
 * - Channel 1: my_pieces (1.0 where current player has pieces, 0.0 elsewhere)
 * - Channel 2: opponent_pieces (1.0 where opponent has pieces, 0.0 elsewhere)
 *
 * This allows the model to learn player-agnostic strategies.
 * Total input size: 2 * boardSize * boardSize
 *
 * @param boardSize The size of the board (e.g., 5 for 5x5)
 */
class GameStateTensorRepresentation(val boardSize: Int)(using TensorScope[NDManager])
    extends BatchableTensorRepresentation[GameState, NDManager] {

  type Tensor = NDArray

  val cellCount: Int = boardSize * boardSize
  val observationSize: Int = 2 * cellCount  // 2 channels: my pieces, opponent pieces

  override def fixedShape: Seq[Int] = Seq(observationSize)

  override def toTensor(a: GameState, scope: NDManager): NDArray = {
    val arr = new Array[Float](observationSize)
    fillGameStateData(a, arr, 0)
    scope.create(arr, new Shape(observationSize))
  }

  override def toBatchedTensor(batch: IndexedSeq[GameState], scope: NDManager): NDArray = {
    val batchSize = batch.size
    val arr = new Array[Float](batchSize * observationSize)

    for (i <- batch.indices) {
      fillGameStateData(batch(i), arr, i * observationSize)
    }
    scope.create(arr, new Shape(batchSize, observationSize))
  }

  /**
   * Fill array with relative encoding:
   * - First cellCount floats: my_pieces (1.0 where current player has pieces)
   * - Next cellCount floats: opponent_pieces (1.0 where opponent has pieces)
   */
  private def fillGameStateData(state: GameState, arr: Array[Float], offset: Int): Unit = {
    val board = state.board
    val currentPlayer = state.nextPlayer
    val opponent = if (currentPlayer == 1) 2 else 1

    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } {
      val cellIndex = i * boardSize + j
      val cellValue = board.getOrElse((i, j), 0)

      // Channel 1: my pieces
      arr(offset + cellIndex) = if (cellValue == currentPlayer) 1.0f else 0.0f

      // Channel 2: opponent pieces
      arr(offset + cellCount + cellIndex) = if (cellValue == opponent) 1.0f else 0.0f
    }
  }

  override def fromTensor(t: NDArray): Option[GameState] = {
    // Reconstruction is lossy - we don't know the original player
    // This is mainly used for debugging, not in training
    val arr = t.toFloatArray
    val board = Board.empty(boardSize)
    for (i <- 0 until boardSize; j <- 0 until boardSize) {
      val cellIndex = i * boardSize + j
      val myPiece = arr(cellIndex)
      val oppPiece = arr(cellCount + cellIndex)
      if (myPiece > 0.5f) board.update(i, j, 1)
      else if (oppPiece > 0.5f) board.update(i, j, 2)
    }
    Some(GameState(board, 1))  // Assume player 1's perspective
  }

  override def fromBatchedTensor(t: NDArray): IndexedSeq[GameState] = {
    val shape = t.getShape
    val batchSize = shape.get(0).toInt
    val arr = t.toFloatArray
    (0 until batchSize).map { batchIdx =>
      val offset = batchIdx * observationSize
      val board = Board.empty(boardSize)
      for (i <- 0 until boardSize; j <- 0 until boardSize) {
        val cellIndex = i * boardSize + j
        val myPiece = arr(offset + cellIndex)
        val oppPiece = arr(offset + cellCount + cellIndex)
        if (myPiece > 0.5f) board.update(i, j, 1)
        else if (oppPiece > 0.5f) board.update(i, j, 2)
      }
      GameState(board, 1)
    }
  }

  override def tensorShape(a: GameState): Seq[Int] = Seq(observationSize)
}

/**
 * CNN-compatible tensor representation for GameState with relative encoding.
 *
 * Encodes the board as a 2D tensor with shape (2, boardSize, boardSize):
 * - Channel 0: my_pieces (1.0 where current player has pieces, 0.0 elsewhere)
 * - Channel 1: opponent_pieces (1.0 where opponent has pieces, 0.0 elsewhere)
 *
 * This format is suitable for convolutional neural networks.
 *
 * @param boardSize The size of the board (e.g., 5 for 5x5)
 */
class GameStateCNNTensorRepresentation(val boardSize: Int)(using TensorScope[NDManager])
    extends BatchableTensorRepresentation[GameState, NDManager] {

  type Tensor = NDArray

  val channels: Int = 2
  val cellCount: Int = boardSize * boardSize
  val totalSize: Int = channels * cellCount

  // Shape for single observation (predictor adds batch dimension automatically)
  override def fixedShape: Seq[Int] = Seq(channels, boardSize, boardSize)

  override def toTensor(a: GameState, scope: NDManager): NDArray = {
    val arr = new Array[Float](totalSize)
    fillGameStateData(a, arr, 0)
    // 3D tensor: (channels, H, W) - predictor adds batch dimension
    scope.create(arr, new Shape(channels, boardSize, boardSize))
  }

  override def toBatchedTensor(batch: IndexedSeq[GameState], scope: NDManager): NDArray = {
    val batchSize = batch.size
    val arr = new Array[Float](batchSize * totalSize)

    for (i <- batch.indices) {
      fillGameStateData(batch(i), arr, i * totalSize)
    }
    scope.create(arr, new Shape(batchSize, channels, boardSize, boardSize))
  }

  /**
   * Fill array with relative encoding in channel-first format:
   * - Channel 0 (offset 0 to cellCount-1): my_pieces
   * - Channel 1 (offset cellCount to 2*cellCount-1): opponent_pieces
   */
  private def fillGameStateData(state: GameState, arr: Array[Float], offset: Int): Unit = {
    val board = state.board
    val currentPlayer = state.nextPlayer
    val opponent = if (currentPlayer == 1) 2 else 1

    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } {
      val cellIndex = i * boardSize + j
      val cellValue = board.getOrElse((i, j), 0)

      // Channel 0: my pieces
      arr(offset + cellIndex) = if (cellValue == currentPlayer) 1.0f else 0.0f

      // Channel 1: opponent pieces
      arr(offset + cellCount + cellIndex) = if (cellValue == opponent) 1.0f else 0.0f
    }
  }

  override def fromTensor(t: NDArray): Option[GameState] = {
    val arr = t.toFloatArray
    val board = Board.empty(boardSize)
    for (i <- 0 until boardSize; j <- 0 until boardSize) {
      val cellIndex = i * boardSize + j
      val myPiece = arr(cellIndex)
      val oppPiece = arr(cellCount + cellIndex)
      if (myPiece > 0.5f) board.update(i, j, 1)
      else if (oppPiece > 0.5f) board.update(i, j, 2)
    }
    Some(GameState(board, 1))
  }

  override def fromBatchedTensor(t: NDArray): IndexedSeq[GameState] = {
    val shape = t.getShape
    val batchSize = shape.get(0).toInt
    val arr = t.toFloatArray
    (0 until batchSize).map { batchIdx =>
      val offset = batchIdx * totalSize
      val board = Board.empty(boardSize)
      for (i <- 0 until boardSize; j <- 0 until boardSize) {
        val cellIndex = i * boardSize + j
        val myPiece = arr(offset + cellIndex)
        val oppPiece = arr(offset + cellCount + cellIndex)
        if (myPiece > 0.5f) board.update(i, j, 1)
        else if (oppPiece > 0.5f) board.update(i, j, 2)
      }
      GameState(board, 1)
    }
  }

  override def tensorShape(a: GameState): Seq[Int] = Seq(channels, boardSize, boardSize)
}
