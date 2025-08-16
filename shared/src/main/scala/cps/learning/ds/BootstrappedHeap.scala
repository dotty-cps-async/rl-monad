package cps.learning.ds

sealed trait BootstrappedHeap[A: Ordering, H[_] : AsHeap] {

  def elementOrdering: Ordering[A] = summon[Ordering[A]]

  def isEmpty: Boolean

  def insert(value: A): BootstrappedHeap[A, H]

  def merge(other: BootstrappedHeap[A, H]): BootstrappedHeap[A, H]

  def findMin: Option[A]

  def deletedMin: BootstrappedHeap[A, H]

}


object BootstrappedHeap {

  case class Empty[A: Ordering, H[_] : AsHeap]() extends BootstrappedHeap[A, H] {

    def isEmpty: Boolean = true

    def insert(value: A): BootstrappedHeap[A, H] = Node(value, summon[AsHeap[H]].empty)

    def merge(other: BootstrappedHeap[A, H]): BootstrappedHeap[A, H] = other

    def findMin: Option[A] = None

    def deletedMin: BootstrappedHeap[A, H] = this

  }

  case class Node[A: Ordering, H[_] : AsHeap](value: A, subheaps: H[BootstrappedHeap[A, H]]) extends BootstrappedHeap[A, H] {

    inline def H = summon[AsHeap[H]]

    def isEmpty: Boolean = false

    def insert(newValue: A): BootstrappedHeap[A, H] = {
      merge(singleton(newValue))
    }

    def merge(other: BootstrappedHeap[A, H]): BootstrappedHeap[A, H] = {
      other match
        case Empty() => this
        case Node(otherValue, otherSubheaps) =>
          if summon[Ordering[A]].lt(value, otherValue) then
            Node(value, H.insert(other, subheaps))
          else
            Node(otherValue, H.insert(this, otherSubheaps))
    }

    def findMin: Option[A] = Some(value)

    def deletedMin: BootstrappedHeap[A, H] = {
      H.findMin(subheaps) match
        case Some(minHeap) =>
          minHeap match
            case Empty() =>
              // impossibe
              throw new IllegalStateException("Deleted min from an empty heap")
            case Node(y, p1) =>
              val p2 = H.deletedMin(subheaps)
              Node(y, H.merge(p1, p2))
        case None =>
          Empty()
    }

  }

  def empty[A: Ordering, H[_] : AsHeap]: BootstrappedHeap[A, H] = Empty()

  def singleton[A: Ordering, H[_] : AsHeap](value: A): BootstrappedHeap[A, H] = Node(value, summon[AsHeap[H]].empty)

  given [H[_] : AsHeap]: AsHeap[[X] =>> BootstrappedHeap[X, H]] with {

    def elementOrdering[A](h: BootstrappedHeap[A, H]): Ordering[A] =
      h.elementOrdering

    def empty[A](using Ordering[A]): BootstrappedHeap[A, H] = BootstrappedHeap.empty[A, H]

    def isEmpty[A](h: BootstrappedHeap[A, H]): Boolean = h.isEmpty

    def insert[A](a: A, heap: BootstrappedHeap[A, H]): BootstrappedHeap[A, H] = heap.insert(a)

    def merge[A](left: BootstrappedHeap[A, H], right: BootstrappedHeap[A, H]): BootstrappedHeap[A, H] = left.merge(right)

    def findMin[A](heap: BootstrappedHeap[A, H]): Option[A] = heap.findMin

    def deletedMin[A](heap: BootstrappedHeap[A, H]): BootstrappedHeap[A, H] = heap.deletedMin

  }

  given [A: Ordering, H[_] : AsHeap]: Ordering[BootstrappedHeap[A, H]] with {
    def compare(x: BootstrappedHeap[A, H], y: BootstrappedHeap[A, H]): Int = (x.findMin, y.findMin) match {
      case (Some(xv), Some(yv)) => summon[Ordering[A]].compare(xv, yv)
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
    }
  }


}

