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
  
  def findMax[A](heap: H[A]): Option[A]

  def deletedMax[A](heap: H[A]): H[A]

  def deueueMax[A](heap: H[A]): (Option[A], H[A]) = {
    given Ordering[A] = elementOrdering(heap)

    val max = findMax(heap)
    max match {
      case Some(value) => (max, deletedMax(heap))
      case None => (max, empty)
    }
  }

}

