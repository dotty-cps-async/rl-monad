package cps.learning.ds

type FingerTreeMinHeap[A] = FingerTree[A, A]

object FingerTreeHeap {

  class MinMonoid[A](ord: Ordering[A], maxInfinity: A) extends FingerTree.Monoid[A] {
    def combine(x: A, y: A): A = ord.min(x, y)

    def zero: A = maxInfinity
  }

  class MaxMonoid[A](ord: Ordering[A], minInfinity: A) extends FingerTree.Monoid[A] {
    def combine(x: A, y: A): A = ord.max(x, y)

    def zero: A = minInfinity
  }

  class MinMaxMonoid[A](ord: Ordering[A], maxInfinity: A, minInfinity: A) extends FingerTree.Monoid[(A, A)] {
    def combine(x: (A, A), y: (A, A)): (A, A) =
      (ord.min(x._1, y._1), ord.max(x._2, y._2))

    def zero: (A, A) = (maxInfinity, minInfinity)
  }

  class MinSizeMonoid[A](ord: Ordering[A], maxInfinity: A, minInfinity: A) extends FingerTree.Monoid[(A, Int)] {
    def combine(x: (A, Int), y: (A, Int)): (A, Int) =
      (ord.min(x._1, y._1), x._2 + y._2)

    def zero: (A, Int) = (maxInfinity, 0)
  }

}
