package cps.rl

import scala.util.{Try, Success, Failure}
import scala.util.control.NonFatal
import scala.util.NotGiven
import cps.*

/**
 * LazyT - a lazy monad transformer that provides stack-safe evaluation
 * for monads that don't natively support deferred execution (like CpsIdentity).
 *
 * Operations are represented as data structures and only evaluated when `run` is called.
 * This provides trampolining to avoid StackOverflowError on deep recursion.
 *
 * @tparam F the underlying monad
 * @tparam A the result type
 */
sealed trait LazyT[F[_], +A]

object LazyT {

  /** Lift a value from the underlying monad */
  case class Lift[F[_], A](fa: F[A]) extends LazyT[F, A]

  /** A pure value */
  case class Pure[F[_], A](value: A) extends LazyT[F, A]

  /** Deferred computation - the key to trampolining */
  case class Delay[F[_], A](thunk: () => LazyT[F, A]) extends LazyT[F, A]

  /** FlatMap operation stored as data */
  case class FlatMap[F[_], A, B](la: LazyT[F, A], f: A => LazyT[F, B]) extends LazyT[F, B]

  /** FlatMapTry operation for error handling */
  case class FlatMapTry[F[_], A, B](la: LazyT[F, A], f: Try[A] => LazyT[F, B]) extends LazyT[F, B]

  /** Error value */
  case class Error[F[_], A](e: Throwable) extends LazyT[F, A]

  /** Create a pure LazyT value */
  def pure[F[_], A](a: A): LazyT[F, A] = Pure(a)

  /** Lift an F[A] into LazyT */
  def lift[F[_], A](fa: F[A]): LazyT[F, A] = Lift(fa)

  /** Create a delayed computation */
  def delay[F[_], A](a: => A): LazyT[F, A] = Delay(() => Pure(a))

  /** Create a delayed LazyT computation */
  def flatDelay[F[_], A](la: => LazyT[F, A]): LazyT[F, A] = Delay(() => la)

  /** Create an error */
  def error[F[_], A](e: Throwable): LazyT[F, A] = Error(e)

  /**
   * Stack-safe interpreter that evaluates LazyT to the underlying monad F.
   * Uses FM.tailRecM for stack-safe iteration, delegating to each monad's
   * own stack-safe implementation.
   */
  def run[F[_], A](lt: LazyT[F, A])(using FM: CpsTryMonad[F]): F[A] = {
    // Type alias for the continuation stack
    type Stack = List[Either[Any => LazyT[F, Any], Try[Any] => LazyT[F, Any]]]
    type State = (LazyT[F, Any], Stack)

    // Apply stack continuations to a successful value
    def applyToValue(value: Any, stack: Stack): F[Either[State, A]] = {
      stack match {
        case Nil =>
          FM.pure(Right(value.asInstanceOf[A]))
        case Left(f) :: rest =>
          val next = try f(value) catch { case NonFatal(e) => Error[F, Any](e) }
          FM.pure(Left((next, rest)))
        case Right(f) :: rest =>
          val next = try f(Success(value)) catch { case NonFatal(e) => Error[F, Any](e) }
          FM.pure(Left((next, rest)))
      }
    }

    // Apply stack continuations to an error
    def applyToError(e: Throwable, stack: Stack): F[Either[State, A]] = {
      stack match {
        case Nil =>
          FM.error(e) // Propagate error through monad
        case Left(_) :: rest =>
          // Skip flatMap continuations, look for flatMapTry
          FM.pure(Left((Error[F, Any](e), rest)))
        case Right(f) :: rest =>
          val next = try f(Failure(e)) catch { case NonFatal(ex) => Error[F, Any](ex) }
          FM.pure(Left((next, rest)))
      }
    }

    // Single step of interpretation
    def step(state: State): F[Either[State, A]] = {
      val (current, stack) = state
      current match {
        case Pure(value) =>
          applyToValue(value, stack)

        case Error(e) =>
          applyToError(e, stack)

        case Delay(thunk) =>
          val next = try thunk() catch { case NonFatal(e) => Error[F, Any](e) }
          FM.pure(Left((next.asInstanceOf[LazyT[F, Any]], stack)))

        case FlatMap(la, f) =>
          FM.pure(Left((la.asInstanceOf[LazyT[F, Any]], Left(f.asInstanceOf[Any => LazyT[F, Any]]) :: stack)))

        case FlatMapTry(la, f) =>
          FM.pure(Left((la.asInstanceOf[LazyT[F, Any]], Right(f.asInstanceOf[Try[Any] => LazyT[F, Any]]) :: stack)))

        case Lift(fa) =>
          // Use FM.flatMapTry to extract value from F[A], handling both success and failure
          FM.flatMapTry(fa.asInstanceOf[F[Any]]) {
            case Success(value) => applyToValue(value, stack)
            case Failure(e) => applyToError(e, stack)
          }
      }
    }

    FM.tailRecM[State, A]((lt.asInstanceOf[LazyT[F, Any]], Nil))(step)
  }

  /**
   * CpsTryEffectMonad instance for LazyT[F, _].
   * This provides delay/flatDelay for deferred evaluation.
   */
  given lazyTCpsEffectMonad[F[_]](using FM: CpsTryMonad[F]): (CpsTryEffectMonad[[X] =>> LazyT[F, X]] & CpsTryMonadInstanceContext[[X] =>> LazyT[F, X]]) =
    new CpsTryEffectMonad[[X] =>> LazyT[F, X]] with CpsTryMonadInstanceContext[[X] =>> LazyT[F, X]] {
      override def pure[A](a: A): LazyT[F, A] = Pure(a)
      override def map[A, B](fa: LazyT[F, A])(f: A => B): LazyT[F, B] =
        FlatMap(fa, (a: A) => Pure(f(a)))
      override def flatMap[A, B](fa: LazyT[F, A])(f: A => LazyT[F, B]): LazyT[F, B] =
        FlatMap(fa, f)
      override def flatMapTry[A, B](fa: LazyT[F, A])(f: Try[A] => LazyT[F, B]): LazyT[F, B] =
        FlatMapTry(fa, f)
      override def error[A](e: Throwable): LazyT[F, A] = Error(e)
      override def delay[A](a: => A): LazyT[F, A] = Delay(() => Pure(a))
      override def flatDelay[A](la: => LazyT[F, A]): LazyT[F, A] = Delay(() => la)
    }

}


/**
 * Type-level dispatch for SuspendableObserver.
 * When F is a CpsTryEffectMonad, use F directly (it has delay/flatDelay).
 * When F is only a CpsTryMonad, use LazyT[F, _] for stack-safe evaluation.
 */
trait SuspendableObserverProvider[F[_]] {
  type SO[X]
  def monad: CpsTryEffectMonad[SO]
  def runSuspended[A](sa: SO[A]): F[A]
  def suspend[A](oa: => F[A]): SO[A]
  def liftPure[A](a: A): SO[A] = monad.pure(a)
}

object SuspendableObserverProvider {

  type Aux[F[_], S[_]] = SuspendableObserverProvider[F] { type SO[X] = S[X] }

  // When F is already an effect monad - use F directly (higher priority)
  given forEffectMonad[F[_]](using em: CpsTryEffectMonad[F]): SuspendableObserverProvider[F] with {
    type SO[X] = F[X]
    def monad: CpsTryEffectMonad[F] = em
    def runSuspended[A](sa: F[A]): F[A] = sa
    def suspend[A](oa: => F[A]): F[A] = em.flatDelay(oa)
  }

  // For non-effect monads (like CpsIdentity) - use LazyT for trampolining
  // The interpreter uses FM.tailRecM for stack-safe iteration, working with any monad
  given forTryMonad[F[_]](using tm: CpsTryMonad[F], nf: NotGiven[CpsTryEffectMonad[F]]): SuspendableObserverProvider[F] with {
    type SO[X] = LazyT[F, X]
    def monad: CpsTryEffectMonad[[X] =>> LazyT[F, X]] = LazyT.lazyTCpsEffectMonad[F]
    def runSuspended[A](sa: LazyT[F, A]): F[A] = LazyT.run(sa)
    def suspend[A](oa: => F[A]): LazyT[F, A] = LazyT.flatDelay(LazyT.lift(oa))
  }
}
