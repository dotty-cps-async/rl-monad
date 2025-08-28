package cps.learning.ds

import munit.*

class AsPriorityQueueSuite extends FunSuite {

  test("AsPriorityQueue with PairingHeap - basic operations") {
    given Ordering[Int] = Ordering.Int

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    given AsPriorityQueue[[A, R] =>> AsPriorityQueue.HeapPair[PairingHeap, A, R], Int] =
      AsPriorityQueue.heapAsPriorityQueue[PairingHeap, Int]

    val pq = summon[AsScaledPriorityQueue[[A, R] =>> AsScaledPriorityQueue.HeapPairWithSize[PairingHeap, A, R], Int]]
    val heap = summon[AsHeap[PairingHeap]]

    val empty = heap.empty[(String, Int)]
    val queue1 = pq.enqueue("task3", 3, empty)
    val queue2 = pq.enqueue("task1", 1, queue1)
    val queue3 = pq.enqueue("task2", 2, queue2)

    assertEquals(pq.peek(queue3), Some("task1"))
    assertEquals(pq.size(queue3), 3)

    val (first, queue4) = pq.dequeue(queue3)
    assertEquals(first, Some("task1"))
    assertEquals(pq.size(queue4), 2)

    val (second, queue5) = pq.dequeue(queue4)
    assertEquals(second, Some("task2"))
    assertEquals(pq.size(queue5), 1)

    val (third, queue6) = pq.dequeue(queue5)
    assertEquals(third, Some("task3"))
    assertEquals(pq.size(queue6), 0)
  }

  test("AsPriorityQueue with PairingHeap - debug heap operations") {
    given Ordering[Int] = Ordering.Int

    given Ordering[(String, Int)] = Ordering.by[(String, Int), Int](_._2)

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    val heap = summon[AsHeap[PairingHeap]]

    val empty = heap.empty[(String, Int)]
    val h1 = heap.insert(("low", 10), empty)
    val h2 = heap.insert(("high", 1), h1)
    val h3 = heap.insert(("medium", 5), h2)

    assertEquals(heap.findMin(h3), Some(("high", 1)))

    val h4 = heap.deletedMin(h3)
    assertEquals(heap.findMin(h4), Some(("medium", 5)))

    val h5 = heap.deletedMin(h4)
    assertEquals(heap.findMin(h5), Some(("low", 10)))
  }

  test("AsPriorityQueue with PairingHeap - empty queue operations") {
    given Ordering[Int] = Ordering.Int

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    given AsPriorityQueue[[A, R] =>> AsPriorityQueue.HeapPair[PairingHeap, A, R], Int] =
      AsPriorityQueue.heapAsPriorityQueue[PairingHeap, Int]

    val pq = summon[AsPriorityQueue[[A, R] =>> AsPriorityQueue.HeapPair[PairingHeap, A, R], Int]]
    val heap = summon[AsHeap[PairingHeap]]

    val empty = heap.empty[(String, Int)]

    assertEquals(pq.peek(empty), None)
    assertEquals(pq.size(empty), 0)

    val (result, stillEmpty) = pq.dequeue(empty)
    assertEquals(result, None)
    assertEquals(pq.size(stillEmpty), 0)
  }

  test("AsPriorityQueue with PairingHeap - priority ordering") {
    given Ordering[Int] = Ordering.Int

    given Ordering[(String, Int)] = Ordering.by[(String, Int), Int](_._2)

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    given AsPriorityQueue[[A, R] =>> AsPriorityQueue.HeapPair[PairingHeap, A, R], Int] =
      AsPriorityQueue.heapAsPriorityQueue[PairingHeap, Int]

    val pq = summon[AsPriorityQueue[[A, R] =>> AsPriorityQueue.HeapPair[PairingHeap, A, R], Int]]
    val heap = summon[AsHeap[PairingHeap]]

    val empty = heap.empty[(String, Int)]
    val queue1 = pq.enqueue("low", 10, empty)
    val queue2 = pq.enqueue("high", 1, queue1)
    val queue3 = pq.enqueue("medium", 5, queue2)

    val (first, queue4) = pq.dequeue(queue3)
    assertEquals(first, Some("high"))

    val (second, queue5) = pq.dequeue(queue4)
    assertEquals(second, Some("medium"))

    val (third, queue6) = pq.dequeue(queue5)
    assertEquals(third, Some("low"))

    val (fourth, _) = pq.dequeue(queue6)
    assertEquals(fourth, None)
  }

  test("AsPriorityQueue with FingerTree - basic operations") {
    given MinMax[Int] = MinMax.given_Min_Int

    given FingerTree.Measured[(String, Int), Int] with {
      def measure(pair: (String, Int)): Int = pair._2
    }
    given FingerTree.Monoid[Int] with {
      def zero: Int = Int.MinValue

      def combine(x: Int, y: Int): Int = math.max(x, y)
    }

    given AsPriorityQueue[[A, R] =>> AsPriorityQueue.FingerTreePair[A, R], Int] =
      AsPriorityQueue.fingerTreeAsPriorityQueue[Int]

    val pq = summon[AsPriorityQueue[[A, R] =>> AsPriorityQueue.FingerTreePair[A, R], Int]]

    val empty = FingerTree.empty[(String, Int), Int]
    val queue1 = pq.enqueue("task3", 3, empty)
    val queue2 = pq.enqueue("task1", 1, queue1)
    val queue3 = pq.enqueue("task2", 2, queue2)

    assertEquals(pq.peek(queue3), Some("task3"))
    assertEquals(pq.size(queue3), 3)

    val (first, queue4) = pq.dequeue(queue3)
    assertEquals(first, Some("task3"))
    assertEquals(pq.size(queue4), 2)

    val (second, queue5) = pq.dequeue(queue4)
    assertEquals(second, Some("task2"))
    assertEquals(pq.size(queue5), 1)

    val (third, queue6) = pq.dequeue(queue5)
    assertEquals(third, Some("task1"))
    assertEquals(pq.size(queue6), 0)
  }

  test("AsPriorityQueue with FingerTree - empty queue operations") {
    given MinMax[Int] = MinMax.given_Min_Int

    given FingerTree.Measured[(String, Int), Int] with {
      def measure(pair: (String, Int)): Int = pair._2
    }
    given FingerTree.Monoid[Int] with {
      def zero: Int = Int.MinValue

      def combine(x: Int, y: Int): Int = math.max(x, y)
    }

    given AsPriorityQueue[[A, R] =>> AsPriorityQueue.FingerTreePair[A, R], Int] =
      AsPriorityQueue.fingerTreeAsPriorityQueue[Int]

    val pq = summon[AsPriorityQueue[[A, R] =>> AsPriorityQueue.FingerTreePair[A, R], Int]]

    val empty = FingerTree.empty[(String, Int), Int]

    assertEquals(pq.peek(empty), None)
    assertEquals(pq.size(empty), 0)

    val (result, stillEmpty) = pq.dequeue(empty)
    assertEquals(result, None)
    assertEquals(pq.size(stillEmpty), 0)
  }

}