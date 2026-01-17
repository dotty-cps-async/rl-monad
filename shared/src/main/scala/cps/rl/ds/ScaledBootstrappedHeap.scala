package cps.rl.ds

import cps.rl.*


type ScaledBootstrappedPairingHeap[A, R] = ScaledBootstrappedHeap[A, R, PairingHeap]

sealed trait ScaledBootstrappedHeap[A: Measured.Curry1[R], R: ScalingGroup : Ordering, H[_] : AsHeap] {

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

  case class Empty[A: Measured.Curry1[R], R: ScalingGroup : Ordering, H[_] : AsHeap]() extends ScaledBootstrappedHeap[A, R, H] {

    def isEmpty: Boolean = true

    override def elementMeasure(elem: A): R = summon[Measured[A, R]].measure(elem)

    def insert(value: A): ScaledBootstrappedHeap[A, R, H] =
      Node(value, summon[AsHeap[H]].empty, summon[ScalingGroup[R]].one)

    def merge(other: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = other

    def findMax: Option[A] = None

    def deletedMax: ScaledBootstrappedHeap[A, R, H] = this

    def scale(factor: R): ScaledBootstrappedHeap[A, R, H] = this

  }

  case class Node[A: Measured.Curry1[R], R: ScalingGroup : Ordering, H[_] : AsHeap](value: A, subheaps: H[ScaledBootstrappedHeap[A, R, H]], factor: R) extends ScaledBootstrappedHeap[A, R, H] {

    inline def H = summon[AsHeap[H]]

    inline def FG = summon[ScalingGroup[R]]

    inline def ord = summon[Ordering[R]]

    inline def M = summon[Measured[A, R]]

    def isEmpty: Boolean = false

    def insert(newValue: A): ScaledBootstrappedHeap[A, R, H] = {
      merge(singleton(newValue))
    }

    def merge(other: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = {
      other match
        case Empty() => this
        case Node(otherValue, otherSubheaps, otherFactor) =>
          if ord.gt(FG.scaleBy(elementMeasure(value), factor), FG.scaleBy(elementMeasure(otherValue), otherFactor)) then
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

  def empty[A: [X] =>> Measured[X, R], R: ScalingGroup : Ordering, H[_] : AsHeap]: ScaledBootstrappedHeap[A, R, H] = Empty()

  def singleton[A: Measured.Curry1[R], R: ScalingGroup : Ordering, H[_] : AsHeap](value: A): ScaledBootstrappedHeap[A, R, H] =
    Node(value, summon[AsHeap[H]].empty, summon[ScalingGroup[R]].one)

  given schMeasured[A: Measured.Curry1[R], R: ScalingGroup : Ordering, H[_] : AsHeap]: Measured[ScaledBootstrappedHeap[A, R, H], R] with {
    def measure(h: ScaledBootstrappedHeap[A, R, H]): R = h.findMax match {
      case Some(v) => summon[Measured[A, R]].measure(v)
      case None => summon[ScalingGroup[R]].zero
    }
  }

  given scaledBootstrappedHeap[H[_] : AsHeap, R: ScalingGroup : Ordering]: AsScaledHeap[[XA, XR] =>> ScaledBootstrappedHeap[XA, XR, H], R] with {

    def elementMeasured[A](heap: ScaledBootstrappedHeap[A, R, H]): Measured[A, R] =
      heap.elementMeasured

    def empty[A](using Measured[A, R]): ScaledBootstrappedHeap[A, R, H] = ScaledBootstrappedHeap.empty[A, R, H]

    def isEmpty[A](h: ScaledBootstrappedHeap[A, R, H]): Boolean = h.isEmpty

    def insert[A](e: A, heap: ScaledBootstrappedHeap[A, R, H]): ScaledBootstrappedHeap[A, R, H] = heap.insert(e)

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

  given [A: Measured.Curry1[R], H[_] : AsHeap, R: ScalingGroup : Ordering]: Ordering[ScaledBootstrappedHeap[A, R, H]] with {
    def compare(x: ScaledBootstrappedHeap[A, R, H], y: ScaledBootstrappedHeap[A, R, H]): Int = (x.findMax, y.findMax) match {
      case (Some(xv), Some(yv)) => summon[Ordering[R]].compare(summon[Measured[A, R]].measure(xv), summon[Measured[A, R]].measure(yv))
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
    }
  }


}

