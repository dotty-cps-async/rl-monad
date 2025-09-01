package cps.learning.ds

/**
 * Logical search policy -
 * defines how to search through alternatives in a logical computation
 *
 * @tparam R
 */
trait LogicalSearchPolicy[R] {

  /**
   * When we unroll a logical stream step, we we
   * i.e.lat wehave computaion of logical strem with score `score`
   * and we extract an element with score `elementScore` which is a logical
   * stream itself.
   *
   * We need to compute the score of the resulting logical stream.
   * By default, this will be just score of element, because we assume
   * that extracting already was processed and the source score is not important.
   * (it's about past, not future).
   *
   * However, in some cases we may want to take into account the source score
   * (for example, we can decrease the score a bit, to reflect that logical stream
   * is deeper in the search tree). Another example is when we do probabilistic
   * programming, and the score is a probability which we somewhat handlde -
   * then we need to multiply
   * the scores.
   *
   * @param score
   * @return
   */
  def unrollStepFactor(elementScore: R, sourceScore: R): R = elementScore

  /**
   * Maximum number of suboptimal results to keep in the pool.
   * If the logical search still produce partial results with better score,
   * and the pool is full, then we return the best result from the pool. If not - we continue searching.
   *
   * If the value is 0, then we return first suboptimal results.
   * If the value is Int.MaxValue or <0, then we always continue searching until the best possible result is found.
   */
  def maxSuboptimalResultPool: Int = 0

  def approxEpsilon: R
}

trait LowLevelDefaultLogicalSearchPolicy {

  given LogicalSearchPolicy[Double] with {
    override def unrollStepFactor(elementScore: Double, sourceScore: Double): Double = elementScore * (1 - 1e-4)

    override def maxSuboptimalResultPool: Int = 100

    override def approxEpsilon: Double = 1e-6
  }

  given LogicalSearchPolicy[Float] with {
    override def unrollStepFactor(elementScore: Float, sourceScore: Float): Float = elementScore * (1 - 1e-4f)

    override def maxSuboptimalResultPool: Int = 100

    override def approxEpsilon: Float = 1e-6f
  }


}

object LogicalSearchPolicy extends LowLevelDefaultLogicalSearchPolicy {

}
