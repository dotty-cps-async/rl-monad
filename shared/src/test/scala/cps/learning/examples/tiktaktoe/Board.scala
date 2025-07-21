package cps.learning.examples.tiktaktoe


type Board = Board.Board


object Board {


   opaque type Board = Map[(Int,Int), Int]

   def empty: Board = Map.empty

   extension (cells: Board) {

    def apply(i: Int, j: Int): Option[Int] = {
      cells.get((i, j))
    }

    def update(i: Int, j: Int, v: Int): Board = {
      cells.updated((i, j), v)
    }

    def isBusy(i: Int, j: Int): Boolean = {
      cells.contains((i, j))
    }

    def isEmpty(i: Int, j: Int): Boolean = {
      !cells.contains((i, j))
    }

    def winner(k: Int): Option[Int] = {
      Board.result(cells, k)
    }

    def isFull(n: Int): Boolean = {
      cells.size == n * n
    }

   }


  /**
   * return Some(winning payer) or None if no one wins.
   * Payer is 1 or 2
   * Paye is winner if exits subset of cells, which are situated near ech other
   *  in row, column or diagonal and all of them are equal to payer.
   * @param b
   * @return
   */
   def result(b: Board, n:Int): Option[Int] = {
     val result = b.find{ case ((i,j),p) =>
       (1 until n).forall(k => b.get((i + k, j    )).contains(p)) ||
       (1 until n).forall(k => b.get((i,     j + k)).contains(p)) ||
       (0 until n).forall(k => b.get((i + k, j + k)).contains(p))
     }
     result.map(_._2)
   }



}

case class Move(i: Int, j: Int, player: Int)


