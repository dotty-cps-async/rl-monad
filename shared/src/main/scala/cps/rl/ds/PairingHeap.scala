package cps.rl.ds

/**
 * A Pairing Heap is a type of heap data structure that supports efficient merging.
 *
 * @tparam A
 */
sealed trait PairingHeap[A: Ordering] {

  def elementOrdering: Ordering[A] = summon[Ordering[A]]

  def isEmpty: Boolean

  def insert(newValue: A): PairingHeap[A]

  def merge(other: PairingHeap[A]): PairingHeap[A]
  
  def findMax: Option[A]

  def deletedMax: PairingHeap[A]
  
}

object PairingHeap {

  case class Empty[A]()(using Ordering[A]) extends PairingHeap[A] {
    override def isEmpty: Boolean = true

    def insert(newValue: A): PairingHeap[A] =
      singleton(newValue)

    def merge(other: PairingHeap[A]): PairingHeap[A] =
      other
    
    def findMax: Option[A] =
      None

    def deletedMax: PairingHeap[A] =
      this

  }

  case class Node[A: Ordering](value: A, children: List[PairingHeap[A]]) extends PairingHeap[A] {

    override def isEmpty: Boolean = false

    def insert(newValue: A): PairingHeap[A] = {
      // Fix: use merge with singleton instead of direct structure manipulation
      merge(singleton(newValue))
    }

    def merge(other: PairingHeap[A]): PairingHeap[A] = {
      other match
        case Empty() => this
        case Node(otherValue, otherChildren) =>
          if summon[Ordering[A]].gt(value, otherValue) then
            Node(value, other :: children)
          else
            Node(otherValue, this :: otherChildren)
    }
    
    def findMax: Option[A] = {
      Some(value)
    }

    def deletedMax: PairingHeap[A] = {
      mergePairs(children)
    }

    def dequeuMax: (Option[A], PairingHeap[A]) = {
      (Some(value), mergePairs(children))
    }


  }

  def empty[A](using Ordering[A]): PairingHeap[A] = Empty()

  def singleton[A](value: A)(using Ordering[A]): PairingHeap[A] = Node(value, Nil)

  def merge[A](x: PairingHeap[A], y: PairingHeap[A])(using Ordering[A]): PairingHeap[A] = {
    x.merge(y)
  }

  def mergePairs[A](children: List[PairingHeap[A]])(using Ordering[A]): PairingHeap[A] = {
    // Standard pairing heap mergePairs algorithm:
    // 1. Left-to-right pass: merge adjacent pairs
    // 2. Right-to-left pass: merge all results together

    @annotation.tailrec
    def pairwiseMerge(heaps: List[PairingHeap[A]], acc: List[PairingHeap[A]]): List[PairingHeap[A]] = {
      heaps match {
        case Nil => acc.reverse
        case h :: Nil => (h :: acc).reverse
        case h1 :: h2 :: tail => pairwiseMerge(tail, h1.merge(h2) :: acc)
      }
    }

    def mergeFromRight(heaps: List[PairingHeap[A]]): PairingHeap[A] = {
      heaps match {
        case Nil => Empty()
        case h :: Nil => h
        case h :: tail => h.merge(mergeFromRight(tail))
      }
    }

    children match {
      case Nil => Empty()
      case _ => mergeFromRight(pairwiseMerge(children, Nil))
    }
  }

  given AsHeap[PairingHeap] with {

    def elementOrdering[A](h: PairingHeap[A]): Ordering[A] = h.elementOrdering

    def empty[A](using Ordering[A]): PairingHeap[A] = PairingHeap.empty[A]

    def isEmpty[A](h: PairingHeap[A]): Boolean = h.isEmpty

    def insert[A](a: A, heap: PairingHeap[A]): PairingHeap[A] = heap.insert(a)

    def merge[A](left: PairingHeap[A], right: PairingHeap[A]): PairingHeap[A] = left.merge(right)
    
    def findMax[A](heap: PairingHeap[A]): Option[A] = heap.findMax

    def deletedMax[A](heap: PairingHeap[A]): PairingHeap[A] = heap.deletedMax

  }

}
