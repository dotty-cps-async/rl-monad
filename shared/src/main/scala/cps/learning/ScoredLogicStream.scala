package cps.learning

import scala.util.Try
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

import cps.CpsTryMonad
import cps.learning.ds.*
import cps.learning.ds.MinMax.given


sealed trait ScoredLogicStreamT[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup] {

  import ScoredLogicStreamT.*

  def applyScore(r: R): ScoredLogicStreamT[F, A, R]

  def maxScore: R

  def |*|(r: R): ScoredLogicStreamT[F, A, R] = applyScore(r)

  def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R]

  def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R]

  def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R]

  def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R]

}

object ScoredLogicStreamT {

  type DefaultQueue[A, R] = ScaledBootstrappedPairingHeap[(A,R), R]

  def asPQ[A, R: LinearlyOrderedGroup]: AsScaledPriorityQueue[DefaultQueue, R] = {
    summon[AsScaledPriorityQueue[DefaultQueue, R]]
  }

  def R[R](using LinearlyOrderedMultiplicativeMonoid[R]) = summon[LinearlyOrderedMultiplicativeMonoid[R]]

  def one[R](using LinearlyOrderedMultiplicativeMonoid[R]) = summon[LinearlyOrderedMultiplicativeMonoid[R]].one


  case class Empty[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup]() extends ScoredLogicStreamT[F, A, R] {


    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] =
      other

    def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = other

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] = this

    def maxScore: R = summon[LinearlyOrderedMultiplicativeMonoid[R]].zero

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Empty()

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Empty()

  }

  case class Pure[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup](value: A, score: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] =
      Pure(value, summon[LinearlyOrderedMultiplicativeMonoid[R]].times(score, r))

    def maxScore: R = score

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      val maxScore = summon[Ordering[R]].compare(score, otherScore) match {
        case c if c <= 0 => otherScore
        case _ => score
      }
      Queue(asPQ.enqueue(other, otherScore, asPQ.enqueue(this, score, asPQ.empty)), one)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      other match
        case Empty() => this
        case Pure(otherValue, otherScore) =>
          Queue(asPQ.enqueue(other, otherScore, asPQ.enqueue(this, score, asPQ.empty)), one)
        case err@Error(e) => err
        case w@WaitF(waited, otherScore) =>
          Queue(asPQ.enqueue(w, otherScore, asPQ.enqueue(this, score, asPQ.empty)), one)
        case s@Suspend(thunk, otherScore) =>
          Queue(asPQ.enqueue(s, otherScore, asPQ.enqueue(this, score, asPQ.empty)), one)
        case q@Queue(pq, multiplier) =>
          Queue(asPQ.enqueue(this, score |/| multiplier, pq), multiplier)
    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] = {
      try
        f(value) |*| score
      catch
        case NonFatal(e) => Error(e)
    }

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] = {
      try
        f(scala.util.Success(value)) |*| score
      catch
        case NonFatal(e) => Error(e)
    }

  }

  case class Error[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup](error: Throwable) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] = this

    def maxScore: R = summon[LinearlyOrderedMultiplicativeMonoid[R]].maxPositiveValue

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] =
      this

    def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      this.asInstanceOf[ScoredLogicStreamT[F, B, R]]

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] = {
      try
        f(Failure(error))
      catch
        case NonFatal(e) => Error(e)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] =
      this


  }

  case class WaitF[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup](waited: F[ScoredLogicStreamT[F, A, R]], score: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] =
      WaitF(waited, summon[LinearlyOrderedMultiplicativeMonoid[R]].times(score, r))

    def maxScore: R = score

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      val maxScore = R.max(score, otherScore)
      Queue(asPQ.enqueue(other, otherScore, asPQ.enqueue(this, score, asPQ.empty)), one)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      WaitF(LM.observerCpsMonad.map(waited)(x => (x |*| score).merge(other)), R.max(score, other.maxScore))

    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      WaitF(summon[CpsTryMonad[F]].flatMap(waited)(s => summon[CpsTryMonad[F]].pure(s.flatMap(f))), score)

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      WaitF(summon[CpsTryMonad[F]].flatMap(waited)(s => summon[CpsTryMonad[F]].pure(s.flatMapTry(f))), score)

  }

  case class Suspend[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup](thunk: () => ScoredLogicStreamT[F, A, R], score: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] =
      Suspend(thunk, R.times(score, r))

    def maxScore: R = score

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      Queue(asPQ.enqueue(other, otherScore, asPQ.enqueue(this, score, asPQ.empty)), R.one)
    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Suspend(() => thunk().flatMap(f), score)

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Suspend(() => thunk().flatMapTry(f), score)

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      other match
        case Empty() => this
        case Pure(otherValue, otherScore) =>
          Queue(asPQ.enqueue(other, otherScore, asPQ.enqueue(this, score, asPQ.empty)), R.one)
        case err@Error(e) => err
        case w@WaitF(waited, otherScore) =>
          Queue(asPQ.enqueue(w, otherScore, asPQ.enqueue(this, score, asPQ.empty)), R.one)
        case s@Suspend(thunk2, otherScore) =>
          Queue(asPQ.enqueue(s, otherScore, asPQ.enqueue(this, score, asPQ.empty)), R.one)
        case q@Queue(pq, multiplier) =>
          Queue(asPQ.enqueue(this, score |/| multiplier, pq), multiplier)

    }

  }


  case class Queue[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup](pq: DefaultQueue[ScoredLogicStreamT[F, A, R], R], multiplier: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] = {
      Queue(pq, R.times(multiplier, r))
    }

    def maxScore: R = {
      asPQ.findMaxPriority(pq) match
        case None => R.zero
        case Some(maxPriority) => R.times(maxPriority, multiplier)
    }

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      asPQ.findMaxPriority(pq) match
        case None =>
          Suspend(() => other, otherScore)
        case Some(maxPriority) =>
          val otherSuspended = Suspend(() => other, otherScore |/| multiplier)
          Queue(asPQ.enqueue(otherSuspended, otherScore, pq), multiplier)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      other match {
        case Empty() => this
        case Pure(otherValue, otherScore) =>
          Queue(asPQ.enqueue(other, otherScore |/| multiplier, pq), multiplier)
        case err@Error(e) => err
        case w@WaitF(waited, otherScore) =>
          Queue(asPQ.enqueue(w, otherScore |/| multiplier, pq), multiplier)
        case s@Suspend(thunk2, otherScore) =>
          Queue(asPQ.enqueue(s, otherScore |/| multiplier, pq), multiplier)
        case q@Queue(otherPq, otherMultiplier) =>
          ???
      }

    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] = {
      val (optFrs, tail) = asPQ.dequeue(pq)
      optFrs match
        case None => Empty()
        case Some(frs) =>
          val waiter = LM.observerCpsMonad.map(LM.fsplit(frs)) {
            case None => Queue(tail, multiplier).flatMap(f)
            case Some((tryHead, rest)) =>
              tryHead match
                case scala.util.Failure(e) =>
                  LM.error[B](e)
                case scala.util.Success(head) =>
                  (f(head) |*| multiplier).merge(Queue(tail, multiplier).flatMap(f))
          }
          WaitF(waiter, R.maxPositiveValue)
    }

  }


  def empty[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup]: ScoredLogicStreamT[F, A, R] = Empty()

  def singleton[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup](value: A, priority: R): ScoredLogicStreamT[F, A, R] = Pure(value, priority)

  class ScoredLogicStreamTMonad[F[_] : CpsTryMonad, R: LinearlyOrderedGroup]
    extends CpsScoredLogicMonadInstanceContext[[A] =>> ScoredLogicStreamT[F, A, R], R] {

    override type Observer[A] = F[A]


    override def pure[T](t: T): ScoredLogicStreamT[F, T, R] = ScoredLogicStreamT.Pure(t, summon[LinearlyOrderedMultiplicativeMonoid[R]].one)

    override def error[T](e: Throwable): ScoredLogicStreamT[F, T, R] = ScoredLogicStreamT.Error(e)

    override def mzero[T]: ScoredLogicStreamT[F, T, R] = ScoredLogicStreamT.empty

    override def scoredPure[A](a: A, score: R): ScoredLogicStreamT[F, A, R] = ScoredLogicStreamT.Pure(a, score)

    override def scoredMplus[A](a: ScoredLogicStreamT[F, A, R], bScore: R, b: => ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] =
      a.scoredMplus(b, bScore)

    override def flatMap[A, B](fa: ScoredLogicStreamT[F, A, R])(f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      fa.flatMap(f)


  }


  given cpsScoredLogicMonad[F[_] : CpsTryMonad, R: LinearlyOrderedGroup]: ScoredLogicStreamTMonad[F, R] =
    new ScoredLogicStreamTMonad[F, R]


  private def LM[F[_] : CpsTryMonad, R: LinearlyOrderedGroup](using lm: ScoredLogicStreamTMonad[F, R]) = lm

  //def emptyQueue

}

