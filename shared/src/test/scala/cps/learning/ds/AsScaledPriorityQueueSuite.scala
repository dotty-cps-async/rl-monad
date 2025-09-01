package cps.learning.ds

import munit.*

class AsScaledPriorityQueueSuite extends FunSuite {

  test("AsScaledPriorityQueue with PairingHeap - basic operations") {


    val pq = summon[AsScaledPriorityQueue[[A, R] =>> ScaledPairingHeap[(A, R), R], Float]]

    val heap = summon[AsScaledHeap[[A, R] =>> ScaledPairingHeap[A, R], Float]]

    val heap1 = ScaledPairingHeap.asScaledHeap[Float]

    //val heap2: AsScaledHeap[A]

    //val empty = heap.empty[(String, Int)]
    val empty = pq.empty[String]
    val queue1 = pq.enqueue("task3", 3, empty)
    val queue2 = pq.enqueue("task1", 1, queue1)
    val queue3 = pq.enqueue("task2", 2, queue2)

    assertEquals(pq.peek(queue3), Some("task3"))

    val (first, queue4) = pq.dequeue(queue3)
    assertEquals(first, Some("task3"))

    val (second, queue5) = pq.dequeue(queue4)
    assertEquals(second, Some("task2"))


    val (third, queue6) = pq.dequeue(queue5)
    assertEquals(third, Some("task1"))
  }

  test("AsPriorityQueue with PairingHeap - debug heap operations") {
    given Ordering[Int] = Ordering.Int

    given Ordering[(String, Int)] = Ordering.by[(String, Int), Int](_._2)

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    val heap = summon[AsHeap[PairingHeap]]

    val empty = heap.empty[(String, Int)]
    val h1 = heap.insert(("low", 1), empty)
    val h2 = heap.insert(("high", 10), h1)
    val h3 = heap.insert(("medium", 5), h2)

    assertEquals(heap.findMax(h3), Some(("high", 10)))

    val h4 = heap.deletedMax(h3)
    assertEquals(heap.findMax(h4), Some(("medium", 5)))

    val h5 = heap.deletedMax(h4)
    assertEquals(heap.findMax(h5), Some(("low", 1)))


  }

  test("AsPriorityQueue with PairingHeap - empty queue operations") {


    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    val pq = summon[AsScaledPriorityQueue[[A, R] =>> ScaledPairingHeap[(A, R), R], Float]]

    val heap = summon[AsScaledHeap[ScaledPairingHeap, Float]]

    given Measured[(String, Float), Float] with {
      def measure(pair: (String, Float)): Float = pair._2
    }

    val empty = heap.empty[(String, Float)]

    assertEquals(pq.peek(empty), None)

    val (result, stillEmpty) = pq.dequeue(empty)
    assertEquals(result, None)
  }

  test("AsScaledPriorityQueue with ScaledPairingHeap - priority ordering") {

    val pq = summon[AsScaledPriorityQueue[[A, R] =>> ScaledPairingHeap[(A, R), R], Float]]


    val empty = pq.empty[String]
    val queue1 = pq.enqueue("low", 1.0, empty)
    val queue2 = pq.enqueue("high", 10.0, queue1)
    val queue3 = pq.enqueue("medium", 5.0, queue2)

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


    val pq = summon[AsScaledPriorityQueue[ScaledMaxFingerTree, Float]]

    val empty = pq.empty[String]
    val queue1 = pq.enqueue("task3", 3, empty)
    val queue2 = pq.enqueue("task1", 1, queue1)
    val queue3 = pq.enqueue("task2", 2, queue2)

    assertEquals(pq.peek(queue3), Some("task3"))

    val (first, queue4) = pq.dequeue(queue3)
    assertEquals(first, Some("task3"))

    val (second, queue5) = pq.dequeue(queue4)
    assertEquals(second, Some("task2"))


    val (third, queue6) = pq.dequeue(queue5)
    assertEquals(third, Some("task1"))
    assert(pq.isEmpty(queue6))
  }

  test("AsPriorityQueue with FingerTree - empty queue operations") {

    val pq = summon[AsScaledPriorityQueue[ScaledMaxFingerTree, Float]]

    val empty = pq.empty[Float]

    assertEquals(pq.peek(empty), None)

    val (result, stillEmpty) = pq.dequeue(empty)
    assertEquals(result, None)
    assert(pq.isEmpty(stillEmpty))
  }

}