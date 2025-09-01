package cps.learning.ds

import cps.learning.*
import cps.learning.LinearlyOrderedGroup


trait AsScaledPriorityQueue[H[_, _], R: LinearlyOrderedGroup] {

  def rOrdering: Ordering[R]

  def empty[A]: H[A, R]

  def isEmpty[A](queue: H[A, R]): Boolean

  def enqueue[A](a: A, priority: R, queue: H[A, R]): H[A, R]

  def dequeue[A](queue: H[A, R]): (Option[A], H[A, R])

  def peek[A](queue: H[A, R]): Option[A]

  def findMaxPriority[A](queue: H[A, R]): Option[R]

  def merge[A](x: H[A, R], y: H[A, R]): H[A, R]

  def scale[A](queue: H[A, R], factor: R): H[A, R]

}

object AsScaledPriorityQueue {

  type Curry[R] = [H[_, _]] =>> AsScaledPriorityQueue[H, R]

  type Curry1[R] = [H[_, _]] =>> AsScaledPriorityQueue[H, R]
  type Curry2[H[_, _]] = [R] =>> AsScaledPriorityQueue[H, R]


  given heapAsScaledPriorityQueue[H[_, _], R](using H: AsScaledHeap[H, R], R: LinearlyOrderedGroup[R]): AsScaledPriorityQueue[[A, R1] =>> H[(A, R1), R1], R] with {


    def rOrdering: Ordering[R] = R

    given pairOrdering[A]: Ordering[(A, R)] = Ordering.by[(A, R), R](_._2)(R)

    given elementMeasure[A]: Measured[(A, R), R] with {
      def measure(pair: (A, R)): R = pair._2
    }

    def empty[A]: H[(A, R), R] = H.empty[(A, R)]

    def isEmpty[A](heap: H[(A, R), R]): Boolean =
      H.isEmpty(heap)

    def enqueue[A](a: A, priority: R, queue: H[(A, R), R]): H[(A, R), R] = {
      H.insert((a, priority), queue)
    }

    def dequeue[A](queue: H[(A, R), R]): (Option[A], H[(A, R), R]) = {
      val (maxPair, newHeap) = H.deueueMax(queue)
      (maxPair.map(_._1), newHeap)
    }

    def peek[A](queue: H[(A, R), R]): Option[A] = {
      H.findMax(queue).map(_._1)
    }

    def findMaxPriority[A](queue: H[(A, R), R]): Option[R] = {
      H.findMax(queue).map(_._2)
    }

    override def merge[A](x: H[(A, R), R], y: H[(A, R), R]): H[(A, R), R] = {
      H.merge(x, y)
    }

    def scale[A](queue: H[(A, R), R], factor: R): H[(A, R), R] = {
      H.scale(queue, factor)
    }

  }

  //type FingerTreePair[A, R] = FingerTree[(A, R), R]

  type ScaledFingerTreeHeap[A, R] = ScaledMaxFingerTree[A, R]

  given fingerTreeAsScaledPriorityQueue[R](using R: LinearlyOrderedGroup[R]): AsScaledPriorityQueue[[A, R] =>> ScaledMaxFingerTree[A, R], R] with {

    def rOrdering: Ordering[R] = summon[LinearlyOrderedGroup[R]]

    private given elementMeasured[A]: Measured[A, R] with {
      def measure(a: A): R = R.one
    }

    override def empty[A]: ScaledFingerTreeHeap[A, R] = {
      val retval: ScaledFingerTreeHeap[A, R] = ScaledMaxFingerTree.empty[A, R]
      retval
    }

    def isEmpty[A](queue: ScaledFingerTreeHeap[A, R]): Boolean = {
      queue.isEmpty
    }

    def enqueue[A](a: A, priority: R, queue: ScaledFingerTreeHeap[A, R]): ScaledFingerTreeHeap[A, R] = {
      queue.appended(ScaledValue(a, priority))
    }

    def dequeue[A](queue: ScaledFingerTreeHeap[A, R]): (Option[A], ScaledFingerTreeHeap[A, R]) = {
      queue.dequeueMax
    }

    def peek[A](queue: ScaledFingerTreeHeap[A, R]): Option[A] = {
      queue.findMax
    }


    override def findMaxPriority[A](queue: ScaledFingerTreeHeap[A, R]): Option[R] = {
      if (queue.isEmpty) then None
      else
        Some(queue.measure)
    }

    override def merge[A](x: ScaledFingerTreeHeap[A, R], y: ScaledFingerTreeHeap[A, R]): ScaledFingerTreeHeap[A, R] = {
      ScaledMaxFingerTree.concat(x, y)
    }

    override def scale[A](queue: ScaledFingerTreeHeap[A, R], factor: R): ScaledFingerTreeHeap[A, R] = {
      queue.scale(factor)
    }

  }

}

extension [H[_, _], A, R](self: H[A, R])(using pq: AsScaledPriorityQueue[H, R]) {

  def enqueue(a: A, priority: R): H[A, R] = pq.enqueue(a, priority, self)
  def dequeue: (Option[A], H[A, R]) = pq.dequeue(self)
  def peek: Option[A] = pq.peek(self)
  def isEmpty: Boolean = pq.isEmpty(self)

}

trait AsSizedScaledPriorityQueue[H[_, _], R] extends AsScaledPriorityQueue[H, R] {

  type Curry1[R] = [H[_, _]] =>> AsScaledPriorityQueue[H, R]
  type Curry2[H[_, _]] = [R] =>> AsScaledPriorityQueue[H, R]

  given heapAsSizedScaledPriorityQueue[H[_, _], R](using heap: AsScaledHeap[H, R], group: LinearlyOrderedGroup[R]): AsScaledPriorityQueue[[A1, R1] =>> HeapPairWithSize[H, A1, R1], R] with {

    def rOrdering: Ordering[R] = group

    given pairOrdering[A]: Ordering[(A, R)] = Ordering.by[(A, R), R](_._2)(group)

    given elementMeasure[A]: Measured[(A, R), R] with {
      def measure(pair: (A, R)): R = pair._2
    }

    def empty[A]: HeapPairWithSize[H, A, R] = HeapPairWithSize(heap.empty, 0)

    def isEmpty[A](queue: HeapPairWithSize[H, A, R]): Boolean =
      heap.isEmpty(queue.heap)

    def enqueue[A](a: A, priority: R, queue: HeapPairWithSize[H, A, R]): HeapPairWithSize[H, A, R] = {
      HeapPairWithSize(heap.insert((a, priority), queue.heap), queue.size + 1)
    }

    def dequeue[A](queue: HeapPairWithSize[H, A, R]): (Option[A], HeapPairWithSize[H, A, R]) = {
      val (maxPair, newHeap) = heap.deueueMax(queue.heap)
      val newSize = if queue.size > 0 then queue.size - 1 else 0
      (maxPair.map(_._1), HeapPairWithSize(newHeap, newSize))
    }

    def peek[A](queue: HeapPairWithSize[H, A, R]): Option[A] = {
      heap.findMax(queue.heap).map(_._1)
    }

    def findMaxPriority[A](queue: HeapPairWithSize[H, A, R]): Option[R] = {
      heap.findMax(queue.heap).map(_._2)
    }

    override def merge[A](x: HeapPairWithSize[H, A, R], y: HeapPairWithSize[H, A, R]): HeapPairWithSize[H, A, R] = {
      HeapPairWithSize(heap.merge(x.heap, y.heap), x.size + y.size)
    }

    def scale[A](queue: HeapPairWithSize[H, A, R], factor: R): HeapPairWithSize[H, A, R] = {
      HeapPairWithSize(heap.scale(queue.heap, factor), queue.size)
    }

    def size[A](queue: HeapPairWithSize[H, A, R]): Int = {
      queue.size
    }

  }


}