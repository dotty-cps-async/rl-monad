package cps.rl.ds

import cps.rl.*

case class ScaledValue[A, R](value: A, factor: R) {
  def scale(l: R)(using ScalingGroup[R]): ScaledValue[A, R] =
    copy(factor = factor |*| l)
}

