package cps.learning.examples.tiktaktoe


case class Board(cells: Board.Cells, size: Int) {

  def apply(i: Int, j: Int): Option[Int] = {
    cells.get((i, j))
  }

  def getOrElse(key: (Int, Int), default: Int): Int = {
    cells.getOrElse(key, default)
  }

  def update(i: Int, j: Int, v: Int): Board = {
    copy(cells = cells.updated((i, j), v))
  }

  def isBusy(i: Int, j: Int): Boolean = {
    cells.contains((i, j))
  }

  def isEmpty(i: Int, j: Int): Boolean = {
    !cells.contains((i, j))
  }

  def winner: Option[Int] = {
    result(size)
  }

  def isFull: Boolean = {
    cells.size == size * size
  }

  /**
   * return Some(winning payer) or None if no one wins.
   * Payer is 1 or 2
   * Paye is winner if exits subset of cells, which are situated near ech other
   * in row, column or diagonal and all of them are equal to payer.
   *
   * @param b
   * @return
   */
  def result(n: Int): Option[Int] = {
    val result = cells.find { case ((i, j), p) =>
      (1 until n).forall(k => cells.get((i + k, j)).contains(p)) ||
        (1 until n).forall(k => cells.get((i, j + k)).contains(p)) ||
        (0 until n).forall(k => cells.get((i + k, j + k)).contains(p))
    }
    result.map(_._2)
  }


}


object Board {

  type Cells = Map[(Int, Int),Int]

  def empty(size: Int): Board = Board(Map.empty, size)

}




case class Move(i: Int, j: Int, player: Int)


