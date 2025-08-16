package cps.learning.ds

import munit.*

class PairingHeapDupSuite extends FunSuite {

  test("PairingHeap - simple insertion and extraction dup") {
    given Ordering[Int] = Ordering.Int

    // Insert just a few elements to see what happens
    val heap1 = PairingHeap.empty[Int]
    val heap2 = heap1.insert(3)
    val heap3 = heap2.insert(1)
    val heap4 = heap3.insert(2)


    val min1 = heap4.findMin
    val remaining1 = heap4.deletedMin

    val min2 = remaining1.findMin
    val remaining2 = remaining1.deletedMin

    val min3 = remaining2.findMin
    val remaining3 = remaining2.deletedMin

    assertEquals(min1, Some(1))
    assertEquals(min2, Some(2))
    assertEquals(min3, Some(3))
    assert(remaining3.isEmpty)
  }

  test("PairingHeap - direct merge behavior dup") {
    given Ordering[Int] = Ordering.Int

    // Create a scenario that would trigger mergePairs
    val heap1 = PairingHeap.singleton(5)
    val heap2 = PairingHeap.singleton(3)
    val heap3 = PairingHeap.singleton(7)
    val heap4 = PairingHeap.singleton(1)

    // Merge them all into one heap
    val merged = heap1.merge(heap2).merge(heap3).merge(heap4)
    println(s"Merged heap: $merged")

    // Now extract minimum - this should trigger mergePairs in deletedMin
    val min = merged.findMin
    println(s"Found min: $min")

    val afterDelete = merged.deletedMin
    println(s"After deletedMin: $afterDelete")

    val min2 = afterDelete.findMin
    println(s"Next min: $min2")
  }

  test("PairingHeap - check for duplication using direct operations") {
    given Ordering[Int] = Ordering.Int

    // Create a more complex heap that might expose the duplication issue
    val values = List(5, 3, 7, 1, 9, 2, 8)
    val heap = values.foldLeft(PairingHeap.empty[Int])((h, v) => h.insert(v))

    println(s"Complex heap: $heap")

    def extractAllDebug(h: PairingHeap[Int], extracted: List[Int] = Nil): List[Int] = {
      if h.isEmpty then {
        println(s"Final extracted: $extracted")
        extracted.reverse
      } else {
        val min = h.findMin
        val remaining = h.deletedMin
        println(s"Extracted: ${min.getOrElse("None")}, remaining: $remaining")
        min match
          case Some(v) => extractAllDebug(remaining, v :: extracted)
          case None => extracted.reverse
      }
    }

    val result = extractAllDebug(heap)
    println(s"All extracted: $result")
    println(s"Expected: ${values.sorted}")

    // Check for duplicates
    val duplicates = result.diff(result.distinct)
    if duplicates.nonEmpty then {
      println(s"Found duplicates: $duplicates")
    }

    assertEquals(result.length, values.length)
    assertEquals(result, values.sorted)
  }
}