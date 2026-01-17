package cps.rl.ds

import munit.*

class PairingHeapDupSuite extends FunSuite {

  test("PairingHeap - simple insertion and extraction dup") {
    given Ordering[Int] = Ordering.Int

    // Insert just a few elements to see what happens
    val heap1 = PairingHeap.empty[Int]
    val heap2 = heap1.insert(3)
    val heap3 = heap2.insert(1)
    val heap4 = heap3.insert(2)


    val max1 = heap4.findMax
    val remaining1 = heap4.deletedMax

    val max2 = remaining1.findMax
    val remaining2 = remaining1.deletedMax

    val max3 = remaining2.findMax
    val remaining3 = remaining2.deletedMax

    assertEquals(max1, Some(3))
    assertEquals(max2, Some(2))
    assertEquals(max3, Some(1))
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

    // Now extract maximul - this should trigger mergePairs in deletedMax
    val max = merged.findMax

    val afterDelete = merged.deletedMax

    val min2 = afterDelete.findMax
  }

  test("PairingHeap - check for duplication using direct operations") {
    given Ordering[Int] = Ordering.Int

    // Create a more complex heap that might expose the duplication issue
    val values = List(5, 3, 7, 1, 9, 2, 8)
    val heap = values.foldLeft(PairingHeap.empty[Int])((h, v) => h.insert(v))

    def extractAllDebug(h: PairingHeap[Int], extracted: List[Int] = Nil): List[Int] = {
      if h.isEmpty then {
        extracted
      } else {
        val max = h.findMax
        val remaining = h.deletedMax
        max match
          case Some(v) => extractAllDebug(remaining, v :: extracted)
          case None => extracted.reverse
      }
    }

    val result = extractAllDebug(heap)

    // Check for duplicates
    val duplicates = result.diff(result.distinct)
    if duplicates.nonEmpty then {
      println(s"Found duplicates: $duplicates")
    }

    assertEquals(result.length, values.length)
    assertEquals(result, values.sorted)
  }
}