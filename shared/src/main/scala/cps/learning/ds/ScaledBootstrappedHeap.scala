package cps.learning.ds

import cps.learning.*
import cps.learning.LinearlyOrderedGroup


type ScaledBootstrappedPairingHeap[A, R] = ScaledBootstrappedHeap[A, R, ScaledPairingHeap]

sealed trait ScaledBootstrappedHeap[A: Measured.Curry1[R], R: LinearlyOrderedGroup, H[_, _] : AsScaledHeap.Curry1[R]] {

  def elementMeasure(elem: A) = summon[Measured[A, R]].measure(elem)

  def elementMeasured: Measured[A, R] = summon[Measured[A, R]]

  def isEmpty: Boolean

  def insert(value: A): ScaledBootstrappedHeap[A, R, H]

  def merge(other: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H]

  def findMax: Option[A]

  def deletedMax: ScaledBootstrappedHeap[A, R, H]

  def scale(factor: R): ScaledBootstrappedHeap[A, R, H]

}


object ScaledBootstrappedHeap {

  case class Empty[A: Measured.Curry1[R], R: LinearlyOrderedGroup, H[_, _] : AsScaledHeap.Curry1[R]]() extends ScaledBootstrappedHeap[A, R, H] {

    def isEmpty: Boolean = true

    override def elementMeasure(elem: A): R = summon[Measured[A, R]].measure(elem)

    def insert(value: A): ScaledBootstrappedHeap[A, R, H] = Node(value, summon[AsScaledHeap[H, R]].empty, summon[LinearlyOrderedGroup[R]].one)

    def merge(other: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = other

    def findMax: Option[A] = None

    def deletedMax: ScaledBootstrappedHeap[A, R, H] = this

    def scale(factor: R): ScaledBootstrappedHeap[A, R, H] = this

  }

  case class Node[A: Measured.Curry1[R], R: LinearlyOrderedGroup, H[_, _] : AsScaledHeap.Curry1[R]](value: A, subheaps: H[ScaledBootstrappedHeap[A, R, H], R], factor: R) extends ScaledBootstrappedHeap[A, R, H] {

    inline def H = summon[AsScaledHeap[H, R]]

    inline def R = summon[LinearlyOrderedGroup[R]]

    inline def M = summon[Measured[A, R]]

    def isEmpty: Boolean = false

    def insert(newValue: A): ScaledBootstrappedHeap[A, R, H] = {
      merge(singleton(newValue))
    }

    def merge(other: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = {
      other match
        case Empty() => this
        case Node(otherValue, otherSubheaps, otherFactor) =>
          if R.gt(elementMeasure(value) |*| factor, elementMeasure(otherValue) |*| otherFactor) then
            Node(value, H.insert(other.scale(otherFactor |/| factor), subheaps), factor)
          else
            Node(otherValue, H.insert(this.scale(factor |/| otherFactor), otherSubheaps), otherFactor)
    }

    def findMax: Option[A] = Some(value)

    def deletedMax: ScaledBootstrappedHeap[A, R, H] = {
      H.findMax(subheaps) match
        case Some(maxHeap) =>
          maxHeap match
            case Empty() =>
              // impossibe
              throw new IllegalStateException("Deleted min from an empty heap")
            case Node(y, p1, yFactor) =>
              val p2 = H.deletedMax(subheaps)
              Node(y, H.merge(p1, p2), factor |*| yFactor)
        case None =>
          Empty()
    }

    def scale(newFactor: R): ScaledBootstrappedHeap[A, R, H] = {
      Node(value, subheaps, factor |*| newFactor)
    }

  }

  def empty[A: [X] =>> Measured[X, R], R: LinearlyOrderedGroup, H[_, _] : AsScaledHeap.Curry1[R]]: ScaledBootstrappedHeap[A, R, H] = Empty()

  def singleton[A: Measured.Curry1[R], R: LinearlyOrderedGroup, H[_, _] : AsScaledHeap.Curry1[R]](value: A): ScaledBootstrappedHeap[A, R, H] =
    Node(value, summon[AsScaledHeap[H, R]].empty, summon[LinearlyOrderedGroup[R]].one)

  given schMeasured[A: Measured.Curry1[R], R: LinearlyOrderedGroup, H[_, _] : AsScaledHeap.Curry1[R]]: Measured[ScaledBootstrappedHeap[A, R, H], R] with {
    def measure(h: ScaledBootstrappedHeap[A, R, H]): R = h.findMax match {
      case Some(v) => summon[Measured[A, R]].measure(v)
      case None => summon[LinearlyOrderedGroup[R]].zero
    }
  }

  given scaledBootstrappedHeap[H[_, _] : AsScaledHeap.Curry1[R], R: LinearlyOrderedGroup]: AsScaledHeap[[XA, XR] =>> ScaledBootstrappedHeap[XA, XR, H], R] with {

    def elementMeasured[A](heap: ScaledBootstrappedHeap[A, R, H]): Measured[A, R] =
      heap.elementMeasured

    def empty[A](using Measured[A, R]): ScaledBootstrappedHeap[A, R, H] = ScaledBootstrappedHeap.empty[A, R, H]

    def isEmpty[A](h: ScaledBootstrappedHeap[A, R, H]): Boolean = h.isEmpty

    def insert[A](a: A, heap: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = heap.insert(a)

    def merge[A](left: ScaledBootstrappedHeap[A, R, H], right: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = left.merge(right)

    def findMax[A](heap: ScaledBootstrappedHeap[A, R, H]): Option[A] = heap.findMax

    def deletedMax[A](heap: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = heap.deletedMax

    def scale[A](heap: ScaledBootstrappedHeap[A, R, H], factor: R): ScaledBootstrappedHeap[A, R, H] =
      heap match
        case Empty() => heap
        case Node(v, subheaps, currentFactor) =>
          given Measured[A, R] = heap.elementMeasured
          Node(v, subheaps, factor |*| currentFactor)

  }

  given [A: Measured.Curry1[R], H[_, _] : AsScaledHeap.Curry1[R], R: LinearlyOrderedGroup]: Ordering[ScaledBootstrappedHeap[A, R, H]] with {
    def compare(x: ScaledBootstrappedHeap[A, R, H], y: ScaledBootstrappedHeap[A, R, H]): Int = (x.findMax, y.findMax) match {
      case (Some(xv), Some(yv)) => summon[LinearlyOrderedGroup[R]].compare(summon[Measured[A, R]].measure(xv), summon[Measured[A, R]].measure(yv))
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
    }
  }


}

