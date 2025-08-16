package cps.learning.ds

/**
 * Heap signature as typeclass.
 * Build from a defintion of Okasaki
 *
 * Chris Okasaki. 1999. Purely Functional Data Structures. Cambridge University Press, USA.
 *
 */
trait AsHeap[H[_]] {

  def elementOrdering[A](h: H[A]): Ordering[A]

  def empty[A](using Ordering[A]): H[A]

  def isEmpty[A](h: H[A]): Boolean

  def insert[A](a: A, heap: H[A]): H[A]

  def merge[A](left: H[A], right: H[A]): H[A]

  def findMin[A](heap: H[A]): Option[A]

  def deletedMin[A](heap: H[A]): H[A]

  def deueueMin[A](heap: H[A]): (Option[A], H[A]) = {
    given Ordering[A] = elementOrdering(heap)

    val min = findMin(heap)
    min match {
      case Some(value) => (min, deletedMin(heap))
      case None => (min, empty)
    }
  }

}

