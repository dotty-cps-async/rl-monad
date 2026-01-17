package cps.rl.ds

import munit.*

class FingerTreeSuite extends FunSuite {

  case class IntWithMax(value: Int)

  given FingerTree.Monoid[IntWithMax] with {
    def zero: IntWithMax = IntWithMax(Int.MinValue)

    def combine(x: IntWithMax, y: IntWithMax): IntWithMax = IntWithMax(math.max(x.value, y.value))
  }

  given Measured[IntWithMax, IntWithMax] with {
    def measure(a: IntWithMax): IntWithMax = a
  }

  given (using Ordering[Int]): Ordering[IntWithMax] with {
    def compare(x: IntWithMax, y: IntWithMax): Int = x.value.compare(y.value)
  }


  test("1.dequeueMax should return element with maximum measure from single element tree") {
    val tree = FingerTree.Single[IntWithMax, IntWithMax](IntWithMax(5))
    val (max, remaining) = tree.dequeueMax

    assertEquals(max, IntWithMax(5))
    assertEquals(remaining.isEmpty, true)
  }


  test("2.dequeueMax should return element with maximum measure from two element tree") {
    val tree = FingerTree.Single[IntWithMax, IntWithMax](IntWithMax(3)).appended(IntWithMax(7))

    val (max, remaining) = tree.dequeueMax

    assertEquals(max, IntWithMax(7))
    assertEquals(remaining.head, IntWithMax(3))
  }


  test("3. dequeueMax should handle tree with multiple elements and return maximum") {
    val tree = FingerTree.Empty[IntWithMax, IntWithMax]()
      .appended(IntWithMax(3))
      .appended(IntWithMax(1))
      .appended(IntWithMax(9))
      .appended(IntWithMax(5))
      .appended(IntWithMax(2))


    val (max, remaining) = tree.dequeueMax


    assertEquals(max, IntWithMax(9))


    // Verify remaining elements are present
    val remainingValues = remaining.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(remainingValues, List(3, 1, 5, 2))
  }

  test("dequeueMax should work correctly with negative values") {
    val tree = FingerTree.Empty[IntWithMax, IntWithMax]()
      .appended(IntWithMax(-5))
      .appended(IntWithMax(-3))
      .appended(IntWithMax(-8))
      .appended(IntWithMax(-1))

    val (max, remaining) = tree.dequeueMax

    assertEquals(max, IntWithMax(-1))

    val remainingValues = remaining.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(remainingValues, List(-5, -3, -8))
  }

  test("dequeueMax should work with duplicate maximum values") {
    val tree = FingerTree.Empty[IntWithMax, IntWithMax]()
      .appended(IntWithMax(5))
      .appended(IntWithMax(3))
      .appended(IntWithMax(5))
      .appended(IntWithMax(1))

    val (max, remaining) = tree.dequeueMax

    assertEquals(max.value, 5)

    // Should still contain the other maximum value
    val remainingValues = remaining.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assert(remainingValues.contains(5))
    assertEquals(remainingValues.length, 3)
  }

  test("dequeueMax should maintain tree structure after removal") {
    val tree = FingerTree.Empty[IntWithMax, IntWithMax]()
      .appended(IntWithMax(4))
      .appended(IntWithMax(2))
      .appended(IntWithMax(8))
      .appended(IntWithMax(6))
      .appended(IntWithMax(1))
      .appended(IntWithMax(3))

    val (max1, tree1) = tree.dequeueMax
    assertEquals(max1, IntWithMax(8))

    val (max2, tree2) = tree1.dequeueMax
    assertEquals(max2, IntWithMax(6))

    val (max3, tree3) = tree2.dequeueMax
    assertEquals(max3, IntWithMax(4))

    assertEquals(tree3.foldLeft(0)((acc, _) => acc + 1), 3) // Should have 3 elements remaining
  }

  test("dequeueMax should work with complex tree structure") {
    // Build a larger tree to test deep structure
    val elements = (1 to 20).map(IntWithMax.apply)
    val tree = elements.foldLeft(FingerTree.empty[IntWithMax, IntWithMax])((acc, elem) => acc.appended(elem))

    val (max, remaining) = tree.dequeueMax

    assertEquals(max, IntWithMax(20))
    assertEquals(remaining.foldLeft(0)((acc, _) => acc + 1), 19)
  }

  test("dequeueMax should preserve order of non-maximum elements") {
    val tree = FingerTree.Empty[IntWithMax, IntWithMax]()
      .appended(IntWithMax(1))
      .appended(IntWithMax(2))
      .appended(IntWithMax(10)) // maximum
      .appended(IntWithMax(3))
      .appended(IntWithMax(4))

    val (max, remaining) = tree.dequeueMax

    assertEquals(max, IntWithMax(10))

    val remainingValues = remaining.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(remainingValues, List(1, 2, 3, 4))
  }

  // Tests for splitTree functionality
  case class IntWithSum(value: Int)

  given sumMonoid: FingerTree.Monoid[IntWithSum] with {
    def zero: IntWithSum = IntWithSum(0)

    def combine(x: IntWithSum, y: IntWithSum): IntWithSum = IntWithSum(x.value + y.value)
  }

  given sumMeasured: Measured[IntWithSum, IntWithSum] with {
    def measure(a: IntWithSum): IntWithSum = a
  }

  test("splitTree should split single element tree") {
    val tree = FingerTree.Single(IntWithSum(5))(using sumMonoid, sumMeasured)
    val split = tree.split(r => r.value > 3)(IntWithSum(0))

    assertEquals(split.pivot, IntWithSum(5))
    assert(split.left.isEmpty)
    assert(split.right.isEmpty)
  }

  test("splitTree should split tree at first element exceeding threshold with sum measure") {
    val tree = FingerTree.empty[IntWithSum, IntWithSum]
      .appended(IntWithSum(2))
      .appended(IntWithSum(3))
      .appended(IntWithSum(4))
      .appended(IntWithSum(1))

    // Split when cumulative sum > 5 (should split after elements with sum = 2+3 = 5, at element 4)
    val split = tree.split(r => r.value > 5)(IntWithSum(0))

    assertEquals(split.pivot, IntWithSum(4))

    val leftValues = split.left.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(leftValues, List(2, 3))

    val rightValues = split.right.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(rightValues, List(1))
  }


  test("splitTree should split at beginning when threshold is very low") {
    val tree = FingerTree.empty[IntWithSum, IntWithSum]
      .appended(IntWithSum(5))
      .appended(IntWithSum(3))
      .appended(IntWithSum(2))

    val split = tree.split(r => r.value > 0)(IntWithSum(0))

    assertEquals(split.pivot, IntWithSum(5))
    assert(split.left.isEmpty)

    val rightValues = split.right.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(rightValues, List(3, 2))
  }

  test("splitTree should split at end when threshold is high") {
    val tree = FingerTree.empty[IntWithSum, IntWithSum]
      .appended(IntWithSum(2))
      .appended(IntWithSum(3))
      .appended(IntWithSum(4))

    val split = tree.split(r => r.value > 8)(IntWithSum(0))

    assertEquals(split.pivot, IntWithSum(4))

    val leftValues = split.left.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(leftValues, List(2, 3))

    assert(split.right.isEmpty)
  }

  test("splitTree should work with complex tree structure") {
    // Build a larger tree to test deeper structure
    val elements = (1 to 15).map(IntWithSum.apply)
    val tree = elements.foldLeft(FingerTree.empty[IntWithSum, IntWithSum])((acc, elem) => acc.appended(elem))

    // Sum of 1..15 is 120, split when cumulative sum > 60
    val split = tree.split(r => r.value > 60)(IntWithSum(0))

    // Should split at some element where cumulative sum first exceeds 60
    val leftSum = split.left.foldLeft(0)((acc, elem) => acc + elem.value)
    val pivotValue = split.pivot.value
    val rightSum = split.right.foldLeft(0)((acc, elem) => acc + elem.value)

    assertEquals(leftSum + pivotValue + rightSum, 120) // Total should be preserved
    assert(leftSum + pivotValue > 60) // First time we exceed threshold
  }

  test("splitTree should work with accumulated offset") {
    val tree = FingerTree.empty[IntWithSum, IntWithSum]
      .appended(IntWithSum(3))
      .appended(IntWithSum(2))
      .appended(IntWithSum(4))

    // Start with offset 5, split when total > 8 (so when tree contribution > 3)
    val split = tree.split(r => r.value > 8)(IntWithSum(5))

    assertEquals(split.pivot, IntWithSum(2))

    val leftValues = split.left.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(leftValues, List(3))

    val rightValues = split.right.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(rightValues, List(4))
  }

  test("splitTree should handle predicate that's never satisfied") {
    val tree = FingerTree.empty[IntWithSum, IntWithSum]
      .appended(IntWithSum(1))
      .appended(IntWithSum(2))
      .appended(IntWithSum(3))

    val split = tree.split(r => r.value > 100)(IntWithSum(0)) // Very high threshold

    assertEquals(split.pivot, IntWithSum(3))

    val leftValues = split.left.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(leftValues, List(1, 2))

    assert(split.right.isEmpty)
  }

  test("splitTree should work with different monoid combinations") {
    // Test with max monoid for splitting
    val tree = FingerTree.empty[IntWithMax, IntWithMax]
      .appended(IntWithMax(3))
      .appended(IntWithMax(7))
      .appended(IntWithMax(2))
      .appended(IntWithMax(9))
      .appended(IntWithMax(1))

    // Split when max so far > 6
    val split = tree.split(r => r.value > 6)(IntWithMax(Int.MinValue))

    assertEquals(split.pivot, IntWithMax(7))

    val leftValues = split.left.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(leftValues, List(3))

    val rightValues = split.right.foldLeft(List.empty[Int])((acc, elem) => elem.value :: acc).reverse
    assertEquals(rightValues, List(2, 9, 1))
  }


}