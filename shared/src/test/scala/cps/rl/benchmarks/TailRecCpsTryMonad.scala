package cps.rl.benchmarks

import scala.util._
import scala.util.control._
import scala.util.control.TailCalls._
import cps._

/**
 * CpsTryMonad instance for TailRec that properly handles errors.
 *
 * Unlike the default CpsThrowMonad[TailRec] which throws immediately,
 * this implementation wraps Try internally to defer error handling.
 */
given CpsTryMonad[TailRec] with CpsTryMonadInstanceContext[TailRec] with {

  def pure[A](a: A): TailRec[A] =
    done(a)

  def map[A, B](fa: TailRec[A])(f: A => B): TailRec[B] =
    fa.map(f)

  def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
    fa.flatMap(f)

  def error[A](e: Throwable): TailRec[A] =
    // For TailRec, we need to throw eventually since there's no Try wrapper
    // But we defer the throw via tailcall to make it stack-safe
    tailcall(done(throw e))

  def flatMapTry[A, B](fa: TailRec[A])(f: Try[A] => TailRec[B]): TailRec[B] =
    // Evaluate fa and wrap result in Try, then apply f
    tailcall {
      val tryResult = Try(fa.result)
      f(tryResult)
    }
}
