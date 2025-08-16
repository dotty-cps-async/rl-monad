package cps.learning.ds

import munit.*

class HeapSuite extends FunSuite {

  def testAsHeapOperations[H[_]](heapName: String)(using AsHeap[H]): Unit = {
    val H = summon[AsHeap[H]]

    test(s"$heapName - empty heap should be empty") {
      given Ordering[Int] = Ordering.Int
      val heap = H.empty[Int]
      assert(H.isEmpty(heap))
      assertEquals(H.findMin(heap), None)
    }

    test(s"$heapName - single element heap") {
      given Ordering[Int] = Ordering.Int
      val heap = H.insert(5, H.empty[Int])
      assert(!H.isEmpty(heap))
      assertEquals(H.findMin(heap), Some(5))
    }

    test(s"$heapName - insert multiple elements and find min") {
      given Ordering[Int] = Ordering.Int
      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)
      val heap4 = H.insert(1, heap3)

      assertEquals(H.findMin(heap4), Some(1))
    }

    test(s"$heapName - deletedMin should remove minimum element") {
      given Ordering[Int] = Ordering.Int
      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)

      assertEquals(H.findMin(heap3), Some(3))
      
      val heap4 = H.deletedMin(heap3)
      assertEquals(H.findMin(heap4), Some(5))
      
      val heap5 = H.deletedMin(heap4)
      assertEquals(H.findMin(heap5), Some(8))
      
      val heap6 = H.deletedMin(heap5)
      assert(H.isEmpty(heap6))
    }

    test(s"$heapName - deueueMin should return min and remaining heap") {
      given Ordering[Int] = Ordering.Int
      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)

      val (min1, remaining1) = H.deueueMin(heap3)
      assertEquals(min1, Some(3))
      assertEquals(H.findMin(remaining1), Some(5))

      val (min2, remaining2) = H.deueueMin(remaining1)
      assertEquals(min2, Some(5))
      assertEquals(H.findMin(remaining2), Some(8))

      val (min3, remaining3) = H.deueueMin(remaining2)
      assertEquals(min3, Some(8))
      assert(H.isEmpty(remaining3))

      val (min4, remaining4) = H.deueueMin(remaining3)
      assertEquals(min4, None)
      assert(H.isEmpty(remaining4))
    }

    test(s"$heapName - merge two heaps") {
      given Ordering[Int] = Ordering.Int
      val heap1 = H.insert(3, H.insert(7, H.empty[Int]))
      val heap2 = H.insert(1, H.insert(5, H.empty[Int]))
      
      val merged = H.merge(heap1, heap2)
      assertEquals(H.findMin(merged), Some(1))
      
      val (_, after1) = H.deueueMin(merged)
      assertEquals(H.findMin(after1), Some(3))
      
      val (_, after2) = H.deueueMin(after1)
      assertEquals(H.findMin(after2), Some(5))
      
      val (_, after3) = H.deueueMin(after2)
      assertEquals(H.findMin(after3), Some(7))
    }

    test(s"$heapName - merge with empty heap") {
      given Ordering[Int] = Ordering.Int
      val heap = H.insert(5, H.empty[Int])
      val empty = H.empty[Int]
      
      val merged1 = H.merge(heap, empty)
      assertEquals(H.findMin(merged1), Some(5))
      
      val merged2 = H.merge(empty, heap)
      assertEquals(H.findMin(merged2), Some(5))
    }

    test(s"$heapName - heap sort using repeated deueueMin") {
      given Ordering[Int] = Ordering.Int
      val values = List(15, 3, 9, 1, 12, 6, 20, 8)
      val heap = values.foldLeft(H.empty[Int])((h, v) => H.insert(v, h))
      
      def extractAll(h: H[Int], acc: List[Int] = Nil): List[Int] = {
        if H.isEmpty(h) then acc.reverse
        else {
          val (min, remaining) = H.deueueMin(h)
          min match
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
      
      assertEquals(H.findMin(heap3), Some("apple"))
      
      val heap4 = H.deletedMin(heap3)
      assertEquals(H.findMin(heap4), Some("banana"))
      
      val heap5 = H.deletedMin(heap4)
      assertEquals(H.findMin(heap5), Some("zebra"))
    }

    test(s"$heapName - heap with custom ordering (reverse)") {
      given Ordering[Int] = Ordering.Int.reverse
      val heap = H.empty[Int]
      val heap1 = H.insert(5, heap)
      val heap2 = H.insert(3, heap1)
      val heap3 = H.insert(8, heap2)
      val heap4 = H.insert(1, heap3)

      // With reverse ordering, max becomes min
      assertEquals(H.findMin(heap4), Some(8))
      
      val heap5 = H.deletedMin(heap4)
      assertEquals(H.findMin(heap5), Some(5))
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
    
    assertEquals(merged.findMin, Some(5))
  }

  // Additional BootstrappedHeap specific tests
  test("BootstrappedHeap - direct instantiation") {
    given Ordering[Int] = Ordering.Int
    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap
    
    val heap1 = BootstrappedHeap.singleton[Int, PairingHeap](10)
    val heap2 = BootstrappedHeap.singleton[Int, PairingHeap](5)
    val merged = heap1.merge(heap2)
    
    assertEquals(merged.findMin, Some(5))
  }

  test("BootstrappedHeap - nested with different heap types") {
    given Ordering[Int] = Ordering.Int
    given AsHeap[PairingHeap] = PairingHeap.given_AsHeap_PairingHeap
    
    type BootstrappedPairingHeap[X] = BootstrappedHeap[X, PairingHeap]
    given AsHeap[BootstrappedPairingHeap] = BootstrappedHeap.given_AsHeap_BootstrappedHeap
    
    type DoublyBootstrapped[X] = BootstrappedHeap[X, BootstrappedPairingHeap]
    
    val H = summon[AsHeap[DoublyBootstrapped]]
    val heap = H.insert(5, H.insert(3, H.insert(8, H.empty[Int])))
    
    assertEquals(H.findMin(heap), Some(3))
  }
}