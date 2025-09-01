package cps.learning.ds

import cps.learning.*

sealed trait ScaledPairingHeap[A: [X] =>> Measured[X, R], R: LinearlyOrderedGroup] {

  def elementMeasured: Measured[A, R] = summon[Measured[A, R]]

  def measureGroup: LinearlyOrderedGroup[R] = summon[LinearlyOrderedGroup[R]]

  def isEmpty: Boolean

  def insert(newValue: A): ScaledPairingHeap[A, R]

  def merge(other: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R]

  def findMax: Option[A]

  def deletedMax: ScaledPairingHeap[A, R]

  def scale(factor: R): ScaledPairingHeap[A, R]

}

object ScaledPairingHeap {

  case class Empty[A: [X] =>> Measured[X, R], R: LinearlyOrderedGroup]() extends ScaledPairingHeap[A, R] {
    override def isEmpty: Boolean = true

    def insert(newValue: A): ScaledPairingHeap[A, R] =
      singleton(newValue)

    def merge(other: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] =
      other

    def findMax: Option[A] =
      None

    def deletedMax: ScaledPairingHeap[A, R] =
      this

    def scale(factor: R): ScaledPairingHeap[A, R] = this

  }

  case class Node[A: [X] =>> Measured[X, R], R: LinearlyOrderedGroup](value: A, children: List[ScaledPairingHeap[A, R]], factor: R) extends ScaledPairingHeap[A, R] {

    override def isEmpty: Boolean = false

    def insert(newValue: A): ScaledPairingHeap[A, R] = {
      // Fix: use merge with singleton instead of direct structure manipulation
      merge(singleton(newValue))
    }

    def merge(other: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] = {
      other match
        case Empty() => this
        case Node(otherValue, otherChildren, otherFactor) =>
          if measureGroup.gt(elementMeasured.measure(value) |*| factor,
            elementMeasured.measure(otherValue) |*| otherFactor) then
            Node(value, other.scale(otherFactor |/| factor) :: children, factor)
          else
            Node(otherValue, this.scale(factor |/| otherFactor) :: otherChildren, otherFactor)
    }

    def findMax: Option[A] = Some(value)

    def deletedMax: ScaledPairingHeap[A, R] = {
      def mergePairs(heaps: List[ScaledPairingHeap[A, R]]): ScaledPairingHeap[A, R] = {
        heaps match
          case Nil => Empty()
          case h1 :: Nil => h1
          case h1 :: h2 :: rest =>
            h1.merge(h2).merge(mergePairs(rest))
      }

      mergePairs(children)
    }

    def scale(factor: R): ScaledPairingHeap[A, R] = {
      Node(value, children, this.factor |*| factor)
    }

  }

  def empty[A, R](using m: Measured[A, R], g: LinearlyOrderedGroup[R]): ScaledPairingHeap[A, R] = Empty()

  def singleton[A, R](value: A)(using m: Measured[A, R], g: LinearlyOrderedGroup[R]): ScaledPairingHeap[A, R] = Node(value, Nil, g.one)

  def merge[A, R](x: ScaledPairingHeap[A, R], y: ScaledPairingHeap[A, R])(using m: Measured[A, R], g: LinearlyOrderedGroup[R]): ScaledPairingHeap[A, R] = {
    x.merge(y)
  }

  given asScaledHeap[R: LinearlyOrderedGroup]: AsScaledHeap[ScaledPairingHeap, R] with {

    def elementMeasured[A](heap: ScaledPairingHeap[A, R]): Measured[A, R] =
      heap.elementMeasured

    def empty[A: Measured.Curry1[R]]: ScaledPairingHeap[A, R] = ScaledPairingHeap.empty

    def isEmpty[A](h: ScaledPairingHeap[A, R]): Boolean = h.isEmpty

    def insert[A](elem: A, heap: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] = heap.insert(elem)

    def merge[A](h1: ScaledPairingHeap[A, R], h2: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] = h1.merge(h2)

    def findMax[A](heap: ScaledPairingHeap[A, R]): Option[A] = heap.findMax

    def deletedMax[A](heap: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] = heap.deletedMax

    def scale[A](heap: ScaledPairingHeap[A, R], factor: R): ScaledPairingHeap[A, R] = heap.scale(factor)

  }

}