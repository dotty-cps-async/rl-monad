package cps.learning

import cps.{CpsMonad, CpsTryMonad}
import cps.monads.logic.*
import cps.syntax.*


trait CpsOrderedLogicMonad[F[_], R:Ordering] extends CpsLogicMonad[F] {

  type Context <: CpsOrderedLogicMonadContext[F, R]

  /**
   * Order the branches of the computation (inside some window) by a scoring function.
   */
  def order[A](m: F[A])(score: A=>R, windowLength: Int = CpsOrderedLogicMonad.DEFAULT_WINDOW_LENGTH): F[A]

  
}

object CpsOrderedLogicMonad {

  type WithOrdering[R] = [F[_]] =>> CpsOrderedLogicMonad[F, R]
  
  val DEFAULT_WINDOW_LENGTH = 100

  
}

extension [F[_]:CpsOrderedLogicMonad.WithOrdering[R], R:Ordering, A](m: F[A]) {

  def orderBy(using monad: CpsOrderedLogicMonad[F, R])(score: A=>R, windowLength: Int = CpsOrderedLogicMonad.DEFAULT_WINDOW_LENGTH): F[A] =
    monad.order(m)(score, windowLength)

}


trait CpsOrderedLogicMonadContext[F[_], R:Ordering] extends CpsLogicMonadContext[F] {

  override def monad: CpsOrderedLogicMonad[F, R]

}

type CpsFloatOrderedLogicMonad[F[_]] = CpsOrderedLogicMonad[F, Float]
type CpsDoubleOrderedLogicMonad[F[_]] = CpsOrderedLogicMonad[F, Double]
