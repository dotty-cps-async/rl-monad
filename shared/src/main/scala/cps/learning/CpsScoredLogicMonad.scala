package cps.learning

import cps.monads.logic.*


/**
 * Monad for weighted logic search strategies,  where we can mark a branch with a score.
 * @tparam F
 */
trait CpsScoredLogicMonad[F[_], R:Ordering] extends CpsOrderedLogicMonad[F, R] {

  type Context <: CpsScoredLogicMonadContext[F, R]

  /**
   * Create the next branch of the computation with a score.
   */
  def scoredMplus[A](m: F[A], scoreN: R, next: => F[A]): F[A]

  /**
   * Create the  branch of the computation according to the scopre.
   */
  def multiScore[A](m: Map[R,()=>F[A]]): F[A] =
    m.foldLeft(empty[A]) { case (acc, (score, next)) =>
      scoredMplus(acc, score, next())
    }
}


trait CpsScoredLogicMonadContext[F[_], R] extends CpsOrderedLogicMonadContext[F, R] {

  override def monad: CpsScoredLogicMonad[F, R]

}

object CpsScoredLogicMonad {
  
  type Curry[R] = [X[_]] =>> CpsScoredLogicMonad[X, R] 
  
  
}

type CpsFloatScoredLogicMonad[F[_]] = CpsScoredLogicMonad[F, Float]
type CpsDoubleScoredLogicMonad[F[_]] = CpsScoredLogicMonad[F, Double]