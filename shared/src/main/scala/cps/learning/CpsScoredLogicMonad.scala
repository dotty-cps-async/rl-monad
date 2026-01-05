package cps.learning

import cps.learning.ds.AsScaledPriorityQueue
import cps.monads.logic.*


/**
 * Monad for weighted logic search strategies,  where we can mark a branch with a score.
 *
 * @tparam F
 */
trait CpsScoredLogicMonad[F[_], R: LinearlyOrderedGroup] extends CpsLogicMonad[F] {

  type Context <: CpsScoredLogicMonadContext[F, R]

  /**
   * Create a pure value with a score.
   */
  def scoredPure[A](a: A, score: R): F[A]

  /**
   * Create the next branch of the computation with a score.
   */
  def scoredMplus[A](m: F[A], scoreNext: R, next: => F[A]): F[A]

  /**
   * Create the  branch of the computation according to the scopre.
   */
  def multiScore[A](m: Seq[(R, () => F[A])]): F[A] =
    m.foldLeft(empty[A]) { case (acc, (score, next)) =>
      scoredMplus(acc, score, next())
    }

  /**
   * Get the first/best result without creating continuation structures.
   * More memory-efficient than fsplit when you don't need the rest of the stream.
   */
  def first[A](fa: F[A]): Observer[Option[scala.util.Try[A]]]
}


trait CpsScoredLogicMonadContext[F[_], R] extends CpsLogicMonadContext[F] {

  override def monad: CpsScoredLogicMonad[F, R]

}

class CpsScoredLogicMonadInstanceContextBody[M[_], R: Ordering](m: CpsScoredLogicMonad[M, R]) extends CpsScoredLogicMonadContext[M, R] {
  override def monad: CpsScoredLogicMonad[M, R] = m
}

trait CpsScoredLogicMonadInstanceContext[M[_], R: Ordering] extends CpsScoredLogicMonad[M, R] {

  override type Context = CpsScoredLogicMonadInstanceContextBody[M, R]

  override def apply[T](op: CpsScoredLogicMonadInstanceContextBody[M, R] => M[T]): M[T] = {
    op(new CpsScoredLogicMonadInstanceContextBody[M, R](this))
  }

}


object CpsScoredLogicMonad {

  type Curry[R] = [X[_]] =>> CpsScoredLogicMonad[X, R]

  type Aux[M[_], C, O[_], R] = CpsScoredLogicMonad[M, R] {
    type Context = C
    type Observer[A] = O[A]
  }


}

type CpsFloatScoredLogicMonad[F[_]] = CpsScoredLogicMonad[F, Float]
type CpsDoubleScoredLogicMonad[F[_]] = CpsScoredLogicMonad[F, Double]