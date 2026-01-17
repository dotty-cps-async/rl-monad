package cps.rl.ds

trait MinMax[R] {
  def ordering: Ordering[R]

  def minValue: R
  def maxValue: R
  def neutralValue: R
}

object MinMax {

  given MinMax[Int] with {
    def ordering: Ordering[Int] = Ordering.Int

    def minValue: Int = Int.MinValue
    def maxValue: Int = Int.MaxValue
    def neutralValue: Int = 0
  }

  given MinMax[Long] with {
    def ordering: Ordering[Long] = Ordering.Long

    def minValue: Long = Long.MinValue
    def maxValue: Long = Long.MaxValue
    def neutralValue: Long = 0L
  }

  given MinMax[Double] with {
    def ordering: Ordering[Double] = summon[Ordering[Double]]

    def minValue: Double = Double.NegativeInfinity
    def maxValue: Double = Double.PositiveInfinity
    def neutralValue: Double = 0.0
  }
  
  given [T](using minMax: MinMax[T]): Ordering[T] = {
    minMax.ordering
  }
  
}

