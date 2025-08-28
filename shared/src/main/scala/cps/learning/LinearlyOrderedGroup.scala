package cps.learning

/**
 *
 *
 */
trait LinearlyOrderedMultiplicativeMonoid[A] extends Ordering[A] {

  def combine(x: A, y: A): A

  def times(x: A, y: A): A = combine(x, y)

  def one: A

  def zero: A = minPositiveValue

  def minPositiveValue: A

  def maxPositiveValue: A
  
  def maxOf(values: A*): A = 
    if values.isEmpty then
      minPositiveValue
    else
      values.reduce((x,y) => if compare(x,y) >= 0 then x else y)

}

extension [A](x: A)
  def |*|(y: A)(using lg: LinearlyOrderedMultiplicativeMonoid[A]): A = lg.combine(x, y)
  def inverse(using lg: LinearlyOrderedGroup[A]): A = lg.inverse(x)
  def |/|(y: A)(using lg: LinearlyOrderedGroup[A]): A = lg.divide(x, y)
  infix def max(y: A)(using lg: LinearlyOrderedMultiplicativeMonoid[A]): A =
    if lg.compare(x, y) >= 0 then x else y
  infix def min(y: A)(using lg: LinearlyOrderedMultiplicativeMonoid[A]): A =
    if lg.compare(x, y) <= 0 then x else y


object LinearlyOrderedMultiplicativeMonoid {

  given LinearlyOrderedMultiplicativeMonoid[Int] with {
    def combine(x: Int, y: Int): Int = Math.abs(x) * Math.abs(y)

    def one: Int = 1

    def minPositiveValue: Int = 0

    def maxPositiveValue: Int = Int.MaxValue

    def compare(x: Int, y: Int): Int = Math.abs(x).compareTo(Math.abs(y))
  }

  given LinearlyOrderedMultiplicativeMonoid[Long] with {
    def combine(x: Long, y: Long): Long = Math.abs(x) * Math.abs(y)

    def one: Long = 1L

    def minPositiveValue: Long = 0L

    def maxPositiveValue: Long = Long.MaxValue

    def compare(x: Long, y: Long): Int = Math.abs(x).compareTo(Math.abs(y))
  }


}

/**
 * Lattice ordered group: adding the inverse
 * see https://ncatlab.org/nlab/show/lattice-ordered+group
 *
 * @tparam A
 */
trait LinearlyOrderedGroup[A] extends LinearlyOrderedMultiplicativeMonoid[A] {

  def divide(x: A, y: A): A

  def inverse(x: A): A = divide(one, x)

}

object LinearlyOrderedGroup {

  given LinearlyOrderedGroup[Double] with {

    def combine(x: Double, y: Double): Double = Math.abs(x) * Math.abs(y)

    def divide(x: Double, y: Double): Double =
      if y == minPositiveValue then
        maxPositiveValue
      else
        Math.abs(x) / Math.abs(y)

    def one: Double = 1.0

    def minPositiveValue: Double = 0.0

    def maxPositiveValue: Double = Double.PositiveInfinity

    def compare(x: Double, y: Double): Int = Math.abs(x).compareTo(Math.abs(y))
  }

  given LinearlyOrderedMultiplicativeMonoid[Float] with {

    def combine(x: Float, y: Float): Float = Math.abs(x) * Math.abs(y)

    def divide(x: Float, y: Float): Float =
      if y == minPositiveValue then
        maxPositiveValue
      else
        Math.abs(x) / Math.abs(y)

    def one: Float = 1.0f

    def minPositiveValue: Float = 0.0f

    def maxPositiveValue: Float = Float.PositiveInfinity

    def compare(x: Float, y: Float): Int = Math.abs(x).compareTo(Math.abs(y))
  }

}

