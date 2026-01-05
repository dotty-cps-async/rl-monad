package cps.learning.examples.tiktaktoe

import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.ndarray.types.Shape
import cps.learning.{TensorScope, BatchableTensorRepresentation}

/**
 * Scoped tensor representation for Board.
 * Supports efficient batching with single memory copy.
 *
 * @param boardSize The size of the board (e.g., 5 for 5x5)
 */
class BoardTensorRepresentation(val boardSize: Int)(using TensorScope[NDManager])
    extends BatchableTensorRepresentation[Board, NDManager] {

  type Tensor = NDArray

  override def fixedShape: Seq[Int] = Seq(boardSize * boardSize)

  override def toTensor(a: Board, scope: NDManager): NDArray = {
    val arr = new Array[Float](boardSize * boardSize)
    fillBoardData(a, arr, 0)
    scope.create(arr, new Shape(boardSize * boardSize))
  }

  override def toBatchedTensor(batch: IndexedSeq[Board], scope: NDManager): NDArray = {
    val batchSize = batch.size
    val obsSize = boardSize * boardSize
    val arr = new Array[Float](batchSize * obsSize)

    for (i <- batch.indices) {
      fillBoardData(batch(i), arr, i * obsSize)
    }
    scope.create(arr, new Shape(batchSize, obsSize))
  }

  private def fillBoardData(board: Board, arr: Array[Float], offset: Int): Unit = {
    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } arr(offset + i * boardSize + j) = board.getOrElse((i, j), 0).toFloat
  }

  override def fromTensor(t: NDArray): Option[Board] = {
    val size = math.sqrt(t.size()).toInt
    val board = Board.empty(size)
    val arr = t.toFloatArray
    for (i <- 0 until size; j <- 0 until size) {
      val value = arr(i * size + j).toInt
      if (value != 0) board.update(i, j, value)
    }
    Some(board)
  }

  override def fromBatchedTensor(t: NDArray): IndexedSeq[Board] = {
    val shape = t.getShape
    val batchSize = shape.get(0).toInt
    val obsSize = boardSize * boardSize
    val arr = t.toFloatArray
    (0 until batchSize).map { i =>
      val board = Board.empty(boardSize)
      for (j <- 0 until boardSize; k <- 0 until boardSize) {
        val value = arr(i * obsSize + j * boardSize + k).toInt
        if (value != 0) board.update(j, k, value)
      }
      board
    }
  }

  override def tensorShape(a: Board): Seq[Int] = Seq(a.size * a.size)
}
