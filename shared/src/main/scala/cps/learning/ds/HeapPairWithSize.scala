package cps.learning.ds

case class HeapPairWithSize[H[_, _], A, R](heap: H[(A, R), R], size: Int)(using H: AsScaledHeap[H, R]) {

  def isEmpty[B]: Boolean = size == 0

  def insert(a: A, priority: R): HeapPairWithSize[H, A, R] =
    HeapPairWithSize(H.insert((a, priority), heap), size + 1)

  def deueueMax: (Option[A], HeapPairWithSize[H, A, R]) = {
    val (optPair, newHeap) = H.deueueMax(heap)
    (optPair.map(_._1), HeapPairWithSize(newHeap, if (optPair.isDefined) size - 1 else size))
  }

}

