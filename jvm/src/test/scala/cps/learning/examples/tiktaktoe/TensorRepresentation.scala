package cps.learning.examples.tiktaktoe

import cps.learning.*

import ai.djl.ndarray.*

given (using NDManager): TensorRepresentation[Board] with {

  type Tensor = NDArray

  def toTensor(a: Board): NDArray = {
    val size = a.size
    val arr = new Array[Float](size * size)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        arr(i * size + j) = a.getOrElse((i, j), 0).toFloat
      }
    }
    val tensor = summon[NDManager].create(arr)
    tensor
  }

  def fromTensor(t: NDArray): Option[Board] = {
    val size = t.getShape.get(0).toInt
    val board = Board.empty(size)
    val arr = t.toFloatArray
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        val value = arr(i * size + j).toInt
        if (value != 0) {
          board.update(i, j, value)
        }
      }
    }
    Some(board)
  }

}
