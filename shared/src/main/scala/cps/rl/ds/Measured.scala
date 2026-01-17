package cps.rl.ds

import cps.rl.ScalingGroup

@FunctionalInterface
trait Measured[A, R] {
  def measure(a: A): R
}

object Measured {

  type Curry1[R] = [A] =>> Measured[A, R]
  type Curry2[A] = [R] =>> Measured[A, R]

}

extension [A, R](a: A)(using m: Measured[A, R])
  def measure: R = m.measure(a)
