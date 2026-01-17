package cps.rl.ds

import cps.rl.ScalingGroup

case class ScaledPriorityQueueWithSize[H[_, _], A, R](heap: H[A, R], size: Int)(using H: AsScaledPriorityQueue[H, R]) {

  def isEmpty: Boolean = size == 0

  def enqueue(a: A, priority: R): ScaledPriorityQueueWithSize[H, A, R] =
    ScaledPriorityQueueWithSize(H.enqueue(a, priority, heap), size + 1)

  def dequeue: (Option[A], ScaledPriorityQueueWithSize[H, A, R]) = {
    val (optA, newHeap) = H.dequeue(heap)
    (optA, ScaledPriorityQueueWithSize(newHeap, if (optA.isDefined) size - 1 else size))
  }

  def peek: Option[A] = H.peek(heap)

  def findMaxPriority: Option[R] = H.findMaxPriority(heap)

  def scale(factor: R): ScaledPriorityQueueWithSize[H, A, R] =
    ScaledPriorityQueueWithSize(H.scale(heap, factor), size)

  def merge(other: ScaledPriorityQueueWithSize[H, A, R]): ScaledPriorityQueueWithSize[H, A, R] =
    ScaledPriorityQueueWithSize(H.merge(heap, other.heap), size + other.size)


}

object ScaledPriorityQueueWithSize {

  def empty[H[_, _], A, R](using H: AsScaledPriorityQueue[H, R]): ScaledPriorityQueueWithSize[H, A, R] =
    ScaledPriorityQueueWithSize(H.empty[A], 0)

  given asSizedScaledPriorityQueue[H[_, _], R: ScalingGroup : Ordering](using AsScaledPriorityQueue[H, R]): AsSizedScaledPriorityQueue[[AX, RX] =>> ScaledPriorityQueueWithSize[H, AX, RX], R] with {

    def rOrdering: Ordering[R] = summon[Ordering[R]]

    def empty[A]: ScaledPriorityQueueWithSize[H, A, R] = ScaledPriorityQueueWithSize.empty[H, A, R]

    def isEmpty[A](queue: ScaledPriorityQueueWithSize[H, A, R]): Boolean = queue.isEmpty

    def enqueue[A](a: A, priority: R, queue: ScaledPriorityQueueWithSize[H, A, R]): ScaledPriorityQueueWithSize[H, A, R] =
      queue.enqueue(a, priority)

    override def dequeue[A](queue: ScaledPriorityQueueWithSize[H, A, R]): (Option[A], ScaledPriorityQueueWithSize[H, A, R]) =
      queue.dequeue

    override def peek[A](queue: ScaledPriorityQueueWithSize[H, A, R]): Option[A] =
      queue.peek

    override def findMaxPriority[A](queue: ScaledPriorityQueueWithSize[H, A, R]): Option[R] =
      queue.findMaxPriority

    override def merge[A](x: ScaledPriorityQueueWithSize[H, A, R], y: ScaledPriorityQueueWithSize[H, A, R]): ScaledPriorityQueueWithSize[H, A, R] =
      x.merge(y)

    override def scale[A](queue: ScaledPriorityQueueWithSize[H, A, R], factor: R): ScaledPriorityQueueWithSize[H, A, R] =
      queue.scale(factor)

    override def size[A](queue: ScaledPriorityQueueWithSize[H, A, R]): Int =
      queue.size

  }

}
