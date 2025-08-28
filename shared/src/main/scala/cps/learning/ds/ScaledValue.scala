package cps.learning.ds

import cps.learning.*

case class ScaledValue[A, R](value: A, factor: R) {
  def scale(l: R)(using LinearlyOrderedGroup[R]): ScaledValue[A, R] =
    copy(factor = factor |*| l)
}

