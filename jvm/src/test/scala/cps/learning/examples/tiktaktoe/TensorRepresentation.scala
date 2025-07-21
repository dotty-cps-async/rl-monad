package cps.learning.examples.tiktaktoe

type Board = Map[(Int,Int), Int]

given (using NDManager)TensorRepresentation[Board] with {
  
  type Tensor = NDArray
  
  def buildTensor(a: Board): NDArray = {
    val size = a.size
    val tensor = summon[NDManager].create(size)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        tensor.set(i, j, a.getOrElse((i, j), 0))
      }
    }
    tensor
  }
  
  def fromTensor(t: NDArray): Option[Board] = {
    val size = t.getShape.get(0).toInt
    val board = Board.empty
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        val value = t.get(i, j).toInt
        if (value != 0) {
          board.update(i, j, value)
        }
      }
    }
    Some(board)
  }

}
