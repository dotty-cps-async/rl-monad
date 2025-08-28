package cps.learning.ds

import cps.learning.LinearlyOrderedGroup

trait AsScaledHeap[H[_, _], R: LinearlyOrderedGroup] {

  def elementMeasured[A](heap: H[A, R]): Measured[A, R]

  def empty[A: Measured.Curry1[R]]: H[A, R]

  def isEmpty[A](h: H[A, R]): Boolean

  def insert[A](elem: A, heap: H[A, R]): H[A, R]

  def merge[A](h1: H[A, R], h2: H[A, R]): H[A, R]

  def findMax[A](heap: H[A, R]): Option[A]

  def deletedMax[A](heap: H[A, R]): H[A, R]

  def deueueMax[A](heap: H[A, R]): (Option[A], H[A, R]) = {
    val max = findMax(heap)
    max match {
      case Some(value) => (max, deletedMax(heap))
      case None => (max, empty(using elementMeasured(heap)))
    }
  }


  def scale[A](heap: H[A, R], factor: R): H[A, R]

}

object AsScaledHeap {

  type Curry1[R] = [H[_, _]] =>> AsScaledHeap[H, R]

  type Curry2[H[_, _]] = [R] =>> AsScaledHeap[H, R]

}