package cps.rl

/**
 * Multiplicative monoid for scaling factors.
 * Uses abs-value to keep factors positive when derived from mixed-sign measures.
 *
 * Note: Does NOT extend Ordering - use standard Ordering[A] for comparing priorities.
 */
trait ScalingMonoid[A] {

  def combine(x: A, y: A): A

  def times(x: A, y: A): A = combine(x, y)

  def one: A

  def zero: A = minPositiveValue

  def minPositiveValue: A

  def maxPositiveValue: A

}

extension [A](x: A)
  def |*|(y: A)(using lg: ScalingMonoid[A]): A = lg.combine(x, y)
  def inverse(using lg: ScalingGroup[A]): A = lg.inverse(x)
  def |/|(y: A)(using lg: ScalingGroup[A]): A = lg.divide(x, y)


object ScalingMonoid {

  given ScalingMonoid[Int] with {
    def combine(x: Int, y: Int): Int = Math.abs(x) * Math.abs(y)

    def one: Int = 1

    def minPositiveValue: Int = 0

    def maxPositiveValue: Int = Int.MaxValue
  }

  given ScalingMonoid[Long] with {
    def combine(x: Long, y: Long): Long = Math.abs(x) * Math.abs(y)

    def one: Long = 1L

    def minPositiveValue: Long = 0L

    def maxPositiveValue: Long = Long.MaxValue
  }

}

/**
 * Scaling group: adding the inverse (division)
 * Uses abs-value to keep factors positive.
 *
 * @tparam A
 */
trait ScalingGroup[A] extends ScalingMonoid[A] {

  def divide(x: A, y: A): A

  def inverse(x: A): A = divide(one, x)

  /**
   * Scale a measure by a positive factor.
   * Unlike combine (|*|), this preserves the sign of the measure.
   * Use this for measure × factor computations where measure can be negative.
   */
  def scaleBy(measure: A, positiveFactor: A): A

}

object ScalingGroup {

  given ScalingGroup[Float] = summon[ScalingRing[Float]]

  given ScalingGroup[Double] = summon[ScalingRing[Double]]

}

trait ScalingRing[A] extends ScalingGroup[A] {

  def add(x: A, y: A): A

  def negate(x: A): A = add(zero, x)

}

/**
 * Additive scaling group where combine is addition.
 * Useful for algorithms with additive costs (like Dijkstra's shortest path).
 *
 * For costs: use negative values, so higher score = lower cost = better.
 *   combine(-cost1, -cost2) = -cost1 + -cost2 = -(cost1 + cost2)
 *
 * Identity is 0 (additive identity), not 1 (multiplicative identity).
 */
trait AdditiveScalingGroup[A] extends ScalingGroup[A] {

  /** Combine via addition */
  override def combine(x: A, y: A): A = add(x, y)

  /** Additive identity */
  override def one: A = additiveZero

  def additiveZero: A

  def add(x: A, y: A): A

  def negate(x: A): A

}

object AdditiveScalingGroup {

  given AdditiveScalingGroup[Float] with {
    def additiveZero: Float = 0.0f

    def add(x: Float, y: Float): Float = x + y

    def negate(x: Float): Float = -x

    def divide(x: Float, y: Float): Float = x - y

    def scaleBy(measure: Float, positiveFactor: Float): Float = measure + positiveFactor

    def minPositiveValue: Float = Float.NegativeInfinity

    def maxPositiveValue: Float = Float.PositiveInfinity
  }

  given AdditiveScalingGroup[Double] with {
    def additiveZero: Double = 0.0

    def add(x: Double, y: Double): Double = x + y

    def negate(x: Double): Double = -x

    def divide(x: Double, y: Double): Double = x - y

    def scaleBy(measure: Double, positiveFactor: Double): Double = measure + positiveFactor

    def minPositiveValue: Double = Double.NegativeInfinity

    def maxPositiveValue: Double = Double.PositiveInfinity
  }

}

object ScalingRing {

  given ScalingRing[Double] with {

    def combine(x: Double, y: Double): Double = Math.abs(x) * Math.abs(y)

    def divide(x: Double, y: Double): Double =
      if y == minPositiveValue then
        maxPositiveValue
      else
        Math.abs(x) / Math.abs(y)

    def scaleBy(measure: Double, positiveFactor: Double): Double = measure * positiveFactor

    def one: Double = 1.0

    def minPositiveValue: Double = 0.0

    def maxPositiveValue: Double = Double.PositiveInfinity

    def add(x: Double, y: Double): Double = x + y

    override def negate(x: Double): Double = -x

  }

  given ScalingRing[Float] with {

    def combine(x: Float, y: Float): Float = Math.abs(x) * Math.abs(y)

    def divide(x: Float, y: Float): Float =
      if y == minPositiveValue then
        maxPositiveValue
      else
        Math.abs(x) / Math.abs(y)

    def scaleBy(measure: Float, positiveFactor: Float): Float = measure * positiveFactor

    def one: Float = 1.0f

    def minPositiveValue: Float = 0.0f

    def maxPositiveValue: Float = Float.PositiveInfinity

    def add(x: Float, y: Float): Float = x + y

    override def negate(x: Float): Float = -x
  }

}
