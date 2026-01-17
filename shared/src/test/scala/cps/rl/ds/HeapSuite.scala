package cps.rl.ds

import munit.*

class HeapSuite extends FunSuite {

  def testAsHeapOperations[H[_]](heapName: String)(using AsHeap[H]): Unit = {
    val H = summon[AsHeap[H]]

    test(s"$heapName - empty heap should be empty") {
      given Ordering[Int] = Ordering.Int

      val heap = H.empty[Int]
      assert(H.isEmpty(heap))
      assertEquals(H.findMax(heap), None)
    }

    test(s"$heapName - single element heap") {
      given Ordering[Int] = Ordering.Int

      val heap = H.insert(5, H.empty[Int])
      assert(!H.isEmpty(heap))
      assertEquals(H.findMax(heap), Some(5))
    }

    test(s"$heapName - insert multiple elements and find max") {
      given Ordering[Int] = Ordering.Int

      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)
      val heap4 = H.insert(1, heap3)

      assertEquals(H.findMax(heap4), Some(8))
    }

    test(s"$heapName - deletedMax should remove maximum element") {
      given Ordering[Int] = Ordering.Int

      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)

      assertEquals(H.findMax(heap3), Some(8))

      val heap4 = H.deletedMax(heap3)
      assertEquals(H.findMax(heap4), Some(5))

      val heap5 = H.deletedMax(heap4)
      assertEquals(H.findMax(heap5), Some(3))

      val heap6 = H.deletedMax(heap5)
      assert(H.isEmpty(heap6))

    }

    test(s"$heapName - deueueMax should return max and remaining heap") {
      given Ordering[Int] = Ordering.Int

      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)

      val (max1, remaining1) = H.deueueMax(heap3)
      assertEquals(max1, Some(8))
      assertEquals(H.findMax(remaining1), Some(5))

      val (max2, remaining2) = H.deueueMax(remaining1)
      
      assertEquals(max2, Some(5))
      assertEquals(H.findMax(remaining2), Some(3))

      val (max3, remaining3) = H.deueueMax(remaining2)
      assertEquals(max3, Some(3))
      assert(H.isEmpty(remaining3))

      val (max4, remaining4) = H.deueueMax(remaining3)
      assertEquals(max4, None)
      assert(H.isEmpty(remaining4))
    }

    test(s"$heapName - merge two heaps") {
      given Ordering[Int] = Ordering.Int

      val heap1 = H.insert(3, H.insert(7, H.empty[Int]))
      val heap2 = H.insert(1, H.insert(5, H.empty[Int]))

      val merged = H.merge(heap1, heap2)
      assertEquals(H.findMax(merged), Some(7))

      val (_, after1) = H.deueueMax(merged)
      assertEquals(H.findMax(after1), Some(5))

      val (_, after2) = H.deueueMax(after1)
      assertEquals(H.findMax(after2), Some(3))

      val (_, after3) = H.deueueMax(after2)
      assertEquals(H.findMax(after3), Some(1))
    }

    test(s"$heapName - merge with empty heap") {
      given Ordering[Int] = Ordering.Int

      val heap = H.insert(5, H.empty[Int])
      val empty = H.empty[Int]

      val merged1 = H.merge(heap, empty)
      assertEquals(H.findMax(merged1), Some(5))

      val merged2 = H.merge(empty, heap)
      assertEquals(H.findMax(merged2), Some(5))
    }

    test(s"$heapName - heap sort using repeated deueueMin") {
      given Ordering[Int] = Ordering.Int

      val values = List(15, 3, 9, 1, 12, 6, 20, 8)
      val heap = values.foldLeft(H.empty[Int])((h, v) => H.insert(v, h))

      def extractAll(h: H[Int], acc: List[Int] = Nil): List[Int] = {
        if H.isEmpty(h) then acc
        else {
          val (max, remaining) = H.deueueMax(h)
          max match
            case Some(value) => extractAll(remaining, value :: acc)
            case None => acc.reverse
        }
      }

      val sortedValues = extractAll(heap)
      assertEquals(sortedValues, values.sorted)
    }

    test(s"$heapName - element ordering consistency") {
      given Ordering[Int] = Ordering.Int

      val heap = H.insert(5, H.empty[Int])
      val ordering = H.elementOrdering(heap)
      assertEquals(ordering, Ordering.Int)
    }

    test(s"$heapName - heap with strings") {
      given Ordering[String] = Ordering.String

      val heap = H.empty[String]
      val heap1 = H.insert("zebra", heap)
      val heap2 = H.insert("apple", heap1)
      val heap3 = H.insert("banana", heap2)

      assertEquals(H.findMax(heap3), Some("zebra"))

      val heap4 = H.deletedMax(heap3)
      assertEquals(H.findMax(heap4), Some("banana"))

      val heap5 = H.deletedMax(heap4)
      assertEquals(H.findMax(heap5), Some("apple"))
    }

    test(s"$heapName - heap with custom ordering (reverse)") {
      given Ordering[Int] = Ordering.Int.reverse

      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)
      val heap4 = H.insert(1, heap3)

      // With reverse ordering, max becomes min
      assertEquals(H.findMax(heap4), Some(1))

      val heap5 = H.deletedMax(heap4)
      assertEquals(H.findMax(heap5), Some(3))
    }
  }

  // Test PairingHeap using AsHeap typeclass
  testAsHeapOperations[PairingHeap]("PairingHeap")

  // Test BootstrappedHeap with PairingHeap as underlying heap
  testAsHeapOperations[[X] =>> BootstrappedHeap[X, PairingHeap]]("BootstrappedHeap[PairingHeap]")

  // Additional PairingHeap specific tests
  test("PairingHeap - direct instantiation and operations") {
    given Ordering[Int] = Ordering.Int

    val heap1 = PairingHeap.singleton(10)
    val heap2 = PairingHeap.singleton(5)
    val merged = heap1.merge(heap2)

    assertEquals(merged.findMax, Some(10))
  }

  // Additional BootstrappedHeap specific tests
  test("BootstrappedHeap - direct instantiation") {
    given Ordering[Int] = Ordering.Int

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    val heap1 = BootstrappedHeap.singleton[Int, PairingHeap](10)
    val heap2 = BootstrappedHeap.singleton[Int, PairingHeap](5)
    val merged = heap1.merge(heap2)

    assertEquals(merged.findMax, Some(10))
  }

  test("BootstrappedHeap - nested with different heap types") {
    given Ordering[Int] = Ordering.Int

    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap

    type BootstrappedPairingHeap[X] = BootstrappedHeap[X, PairingHeap]

    given AsHeap[BootstrappedPairingHeap] = BootstrappedHeap.given_AsHeap_BootstrappedHeap

    type DoublyBootstrapped[X] = BootstrappedHeap[X, BootstrappedPairingHeap]

    val H = summon[AsHeap[DoublyBootstrapped]]
    val heap = H.insert(5, H.insert(3, H.insert(8, H.empty[Int])))

    assertEquals(H.findMax(heap), Some(8))
  }
}