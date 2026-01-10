package cps.learning.ds

import scala.annotation.tailrec
import cps.learning.*

sealed trait ScaledPairingHeap[A: [X] =>> Measured[X, R], R: ScalingGroup : Ordering] {

  def elementMeasured: Measured[A, R] = summon[Measured[A, R]]

  def factorGroup: ScalingGroup[R] = summon[ScalingGroup[R]]

  def ordering: Ordering[R] = summon[Ordering[R]]

  def isEmpty: Boolean

  def insert(newValue: A): ScaledPairingHeap[A, R]

  def merge(other: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R]

  def findMax: Option[A]

  def deletedMax: ScaledPairingHeap[A, R]

  def scale(factor: R): ScaledPairingHeap[A, R]

}

object ScaledPairingHeap {

  case class Empty[A: [X] =>> Measured[X, R], R: ScalingGroup : Ordering]() extends ScaledPairingHeap[A, R] {
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

  case class Node[A: [X] =>> Measured[X, R], R: ScalingGroup : Ordering](value: A, children: List[ScaledPairingHeap[A, R]], factor: R) extends ScaledPairingHeap[A, R] {

    override def isEmpty: Boolean = false

    def insert(newValue: A): ScaledPairingHeap[A, R] = {
      // Fix: use merge with singleton instead of direct structure manipulation
      merge(singleton(newValue))
    }

    def merge(other: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] = {
      other match
        case Empty() => this
        case Node(otherValue, otherChildren, otherFactor) =>
          if ordering.gt(factorGroup.scaleBy(elementMeasured.measure(value), factor),
            factorGroup.scaleBy(elementMeasured.measure(otherValue), otherFactor)) then
            Node(value, other.scale(otherFactor |/| factor) :: children, factor)
          else
            Node(otherValue, this.scale(factor |/| otherFactor) :: otherChildren, otherFactor)
    }

    def findMax: Option[A] = Some(value)

    def deletedMax: ScaledPairingHeap[A, R] = {
      // Tail-recursive version of mergePairs to avoid stack overflow
      // First pass: merge adjacent pairs, accumulate results
      // Second pass: merge all accumulated heaps
      @tailrec
      def mergePairsAcc(heaps: List[ScaledPairingHeap[A, R]], acc: List[ScaledPairingHeap[A, R]]): List[ScaledPairingHeap[A, R]] = {
        heaps match
          case Nil => acc
          case h1 :: Nil => h1 :: acc
          case h1 :: h2 :: rest => mergePairsAcc(rest, h1.merge(h2) :: acc)
      }

      @tailrec
      def mergeAll(heaps: List[ScaledPairingHeap[A, R]], acc: ScaledPairingHeap[A, R]): ScaledPairingHeap[A, R] = {
        heaps match
          case Nil => acc
          case h :: rest => mergeAll(rest, acc.merge(h))
      }

      mergeAll(mergePairsAcc(children, Nil), Empty())
    }

    def scale(factor: R): ScaledPairingHeap[A, R] = {
      Node(value, children, this.factor |*| factor)
    }

  }

  def empty[A, R](using m: Measured[A, R], g: ScalingGroup[R], ord: Ordering[R]): ScaledPairingHeap[A, R] = Empty()

  def singleton[A, R](value: A)(using m: Measured[A, R], g: ScalingGroup[R], ord: Ordering[R]): ScaledPairingHeap[A, R] = Node(value, Nil, g.one)

  def merge[A, R](x: ScaledPairingHeap[A, R], y: ScaledPairingHeap[A, R])(using m: Measured[A, R], g: ScalingGroup[R], ord: Ordering[R]): ScaledPairingHeap[A, R] = {
    x.merge(y)
  }

  given asScaledHeap[R: ScalingGroup : Ordering]: AsScaledHeap[ScaledPairingHeap, R] with {

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
