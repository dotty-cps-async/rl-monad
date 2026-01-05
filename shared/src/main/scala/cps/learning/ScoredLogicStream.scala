package cps.learning

import scala.util.Try
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import cps.*
import cps.syntax.{flatMap, map}
import cps.learning.ds.*
import cps.learning.ds.MinMax.given


sealed trait ScoredLogicStreamT[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy] {

  import ScoredLogicStreamT.*

  def applyScore(r: R): ScoredLogicStreamT[F, A, R]

  def maxScore: R

  def |*|(r: R): ScoredLogicStreamT[F, A, R] = applyScore(r)

  def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R]

  def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R]

  def map[B](f: A => B): ScoredLogicStreamT[F, B, R] = flatMap(a => Pure(f(a), one))

  def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R]

  def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R]

  def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]]

  def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R]

  /**
   * Get only the first/best result without creating continuation structures.
   * This is more memory-efficient than fsplit when you don't need the rest of the stream.
   */
  def first: F[Option[Try[A]]]

}

object ScoredLogicStreamT {

  type DefaultQueue[A, R] = ScaledBootstrappedPairingHeap[(A, R), R]

  type DefaultSizedQueue[A, R] = ScaledPriorityQueueWithSize[DefaultQueue, A, R]

  def asPQ[A, R: LinearlyOrderedGroup]: AsScaledPriorityQueue[DefaultQueue, R] = {
    summon[AsScaledPriorityQueue[DefaultQueue, R]]
  }

  def asSSPQ[A, R: LinearlyOrderedGroup]: AsSizedScaledPriorityQueue[DefaultSizedQueue, R] = {
    summon[AsSizedScaledPriorityQueue[DefaultSizedQueue, R]]
  }

  def R[R](using LinearlyOrderedMultiplicativeMonoid[R]) = summon[LinearlyOrderedMultiplicativeMonoid[R]]

  def one[R](using LinearlyOrderedMultiplicativeMonoid[R]) = summon[LinearlyOrderedMultiplicativeMonoid[R]].one

  def lsPolicy[R](using lp: LogicalSearchPolicy[R]) = lp

  case class Empty[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy]() extends ScoredLogicStreamT[F, A, R] {


    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] =
      other

    def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = other

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] = this

    def maxScore: R = summon[LinearlyOrderedMultiplicativeMonoid[R]].zero

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Empty()

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Empty()

    override def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] =
      summon[CpsTryMonad[F]].pure(None)

    override def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] = {
      Pure(None, R.one)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].pure(None)

  }

  case class Pure[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy](value: A, score: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] =
      Pure(value, summon[LinearlyOrderedMultiplicativeMonoid[R]].times(score, r))

    def maxScore: R = score

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      val maxScore = summon[Ordering[R]].compare(score, otherScore) match {
        case c if c <= 0 => otherScore
        case _ => score
      }
      Queue(asPQ.enqueue(other, otherScore, asPQ.enqueue(this, score, asPQ.empty)), asSSPQ.empty)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      other match
        case Empty() => this
        case Pure(otherValue, otherScore) =>
          Queue.empty.enqueue(this, score).enqueue(other, otherScore)
        case err@Error(e) => err
        case w@WaitF(waited, otherScore) =>
          Queue.empty.enqueue(this, score).enqueue(other, otherScore)
        case s@Suspend(thunk, otherScore) =>
          Queue.empty.enqueue(this, score).enqueue(other, otherScore)
        case q@Queue(_, _) =>
          q.enqueue(this, score)
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

    override def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] =
      summon[CpsTryMonad[F]].pure(Some((Success(value), Empty())))

    override def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] = {
      Pure(Some((Success(value), Empty())), R.one |*| score)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].pure(Some(Success(value)))

  }

  case class Error[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy](error: Throwable) extends ScoredLogicStreamT[F, A, R] {

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

    override def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] =
      summon[CpsTryMonad[F]].pure(Some((Failure(error), Empty())))

    override def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] = {
      Pure(Some((Failure(error), Empty())), R.one |*| maxScore)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].pure(Some(Failure(error)))

  }

  case class WaitF[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy](waited: F[ScoredLogicStreamT[F, A, R]], score: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] =
      WaitF(waited, summon[LinearlyOrderedMultiplicativeMonoid[R]].times(score, r))

    def maxScore: R = score

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      val maxScore = R.max(score, otherScore)
      Queue.empty.enqueue(this, score).enqueue(Suspend(() => other, otherScore), otherScore)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      WaitF(LM.observerCpsMonad.map(waited)(x => (x |*| score).merge(other)), R.max(score, other.maxScore))
    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      WaitF(summon[CpsTryMonad[F]].flatMap(waited)(s => summon[CpsTryMonad[F]].pure(s.flatMap(f))), score)

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      WaitF(summon[CpsTryMonad[F]].flatMap(waited)(s => summon[CpsTryMonad[F]].pure(s.flatMapTry(f))), score)

    override def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] =
      summon[CpsTryMonad[F]].flatMap(waited) { s =>
        summon[CpsTryMonad[F]].map(s.fsplit) {
          case None => None
          case Some((tryHead, rest)) =>
            Some((tryHead, rest))
        }
      }

    override def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] = {
      WaitF(summon[CpsTryMonad[F]].map(waited) { s =>
        s.msplit
      }, score)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].flatMap(waited)(_.first)

  }

  case class Suspend[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy](
                                                                                            thunk: () => ScoredLogicStreamT[F, A, R], score: R) extends ScoredLogicStreamT[F, A, R] {

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] =
      Suspend(thunk, R.times(score, r))

    def maxScore: R = score

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      Queue.empty.enqueue(this, score).enqueue(Suspend(() => other, otherScore), otherScore)
    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Suspend(() => thunk().flatMap(f), score)

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      Suspend(() => thunk().flatMapTry(f), score)

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      other match
        case Empty() => this
        case p@Pure(otherValue, otherScore) =>
          Queue(asPQ.enqueue(this, score, asPQ.empty), asSSPQ.enqueue(p, otherScore, asSSPQ.empty))
        case err@Error(e) => err
        case w@WaitF(waited, otherScore) =>
          Queue(asPQ.enqueue(w, otherScore, asPQ.enqueue(this, score, asPQ.empty)), asSSPQ.empty)
        case s@Suspend(thunk2, otherScore) =>
          Queue(asPQ.enqueue(s, otherScore, asPQ.enqueue(this, score, asPQ.empty)), asSSPQ.empty)
        case q: Queue[F, A, R] =>
          q.enqueue(this, score)
    }

    override def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] =
      try {
        thunk().fsplit
      } catch {
        case NonFatal(e) =>
          summon[CpsTryMonad[F]].pure(Some((Failure(e), Empty())))
      }

    override def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] = {
      Suspend(() => thunk().msplit, score)
    }

    override def first: F[Option[Try[A]]] =
      try {
        thunk().first
      } catch {
        case NonFatal(e) =>
          summon[CpsTryMonad[F]].pure(Some(Failure(e)))
      }

  }


  case class Queue[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy](streams: DefaultQueue[ScoredLogicStreamT[F, A, R], R], resultPool: DefaultSizedQueue[Pure[F, A, R], R]) extends ScoredLogicStreamT[F, A, R] {

    def enqueue(other: ScoredLogicStreamT[F, A, R], otherScore: R): Queue[F, A, R] = {
      Queue(asPQ.enqueue(other, otherScore, streams), resultPool)
    }

    def applyScore(r: R): ScoredLogicStreamT[F, A, R] = {
      Queue(streams.scale(r), resultPool.scale(r))
    }

    def dequeue: Option[(ScoredLogicStreamT[F, A, R], Queue[F, A, R])] = {
      resultPool.findMaxPriority match {
        case Some(rpPrirority) =>
          asPQ.findMaxPriority(streams) match
            case Some(streamsPriority) =>
              if (summon[Ordering[R]].gteq(rpPrirority, streamsPriority)) then
                val (Some(frs), tailPool) = resultPool.dequeue: @unchecked
                Some(frs, Queue(streams, tailPool))
              else
                val (Some(frs), tail) = asPQ.dequeue(streams): @unchecked
                Some((frs, Queue(tail, resultPool)))
            case None =>
              val (Some(frs), tailPool) = resultPool.dequeue: @unchecked
              Some(frs, Queue(streams, tailPool))
        case None =>
          // resultPool is empty
          val (optFrs, tail) = asPQ.dequeue(streams)
          optFrs match
            case None =>
              None
            case Some(frs) =>
              Some((frs, Queue(tail, resultPool)))
      }
    }

    def maxScore: R = {
      asPQ.findMaxPriority(streams) match
        case None =>
          resultPool.findMaxPriority match {
            case None => R.zero
            case Some(rpMaxPriority) => rpMaxPriority
          }
        case Some(maxPriority) =>
          resultPool.findMaxPriority match {
            case None => maxPriority
            case Some(rpMaxPriority) => R.max(maxPriority, rpMaxPriority)
          }
    }

    def scoredMplus(other: => ScoredLogicStreamT[F, A, R], otherScore: R): ScoredLogicStreamT[F, A, R] = {
      asPQ.findMaxPriority(streams) match
        case None =>
          Suspend(() => other, otherScore)
        case Some(maxPriority) =>
          val otherSuspended = Suspend(() => other, otherScore)
          Queue(asPQ.enqueue(otherSuspended, otherScore, streams), asSSPQ.empty)
    }

    override def merge(other: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] = {
      other match {
        case Empty() => this
        case Pure(otherValue, otherScore) =>
          enqueue(other, otherScore)
        case err@Error(e) => err
        case w@WaitF(waited, otherScore) =>
          enqueue(w, otherScore)
        case s@Suspend(thunk2, otherScore) =>
          enqueue(s, otherScore)
        case q@Queue(otherStreams, otherResultPool) =>
          Queue(asPQ.merge(streams, otherStreams), asSSPQ.merge(resultPool, otherResultPool))
      }

    }

    override def flatMap[B](f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] = {
      dequeue match
        case None => Empty()
        case Some((frs, tail)) =>
          // Keep tail lazy to avoid forcing all queued branches at once
          frs.flatMap(f).merge(Suspend(() => tail.flatMap(f), tail.maxScore))
    }

    override def flatMapTry[B](f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] = {
      dequeue match {
        case None => Empty()
        case Some((frs, tail)) =>
          // Keep tail lazy to avoid forcing all queued branches at once
          frs.flatMapTry(f).merge(Suspend(() => tail.flatMapTry(f), tail.maxScore))
      }
    }

    override def fsplit: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] = {

      def getFromResultPool: F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] = {
        resultPool.dequeue match
          case (Some(frs), tailPool) =>
            summon[CpsTryMonad[F]].pure(Some((Success(frs.value), Queue(streams, tailPool))))
          case (None, _) =>
            summon[CpsTryMonad[F]].pure(None)
      }

      def getFromStream(backupFromResultPool: Boolean): F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] = {
        val (optFrs, tail) = asPQ.dequeue(streams)
        optFrs match
          case None =>
            if backupFromResultPool then
              getFromResultPool
            else
              summon[CpsTryMonad[F]].pure(None)
          case Some(frs) =>
            frs match
              case Empty() =>
                Queue(tail, resultPool).fsplit
              case p@Pure(value, score) =>
                summon[CpsTryMonad[F]].pure(Some((Success(value), Queue(tail, resultPool))))
              case err@Error(e) =>
                summon[CpsTryMonad[F]].pure(Some((Failure(e), Queue(tail, resultPool))))
              case w@WaitF(waited, score) =>
                waited.flatMap { s =>
                  s.fsplit
                }.flatMap {
                  case None => Queue(tail, resultPool).fsplit
                  case Some((tryHead, rest)) =>
                    summon[CpsTryMonad[F]].pure(Some((tryHead, rest.merge(Queue(tail, resultPool)))))
                }
              case s@Suspend(thunk, score) =>
                try
                  thunk().fsplit.flatMap {
                    case None => Queue(tail, resultPool).fsplit
                    case Some((tryHead, rest)) =>
                      summon[CpsTryMonad[F]].pure(Some((tryHead, rest.merge(Queue(tail, resultPool)))))
                  }
                catch
                  case NonFatal(e) =>
                    summon[CpsTryMonad[F]].pure(Some((Failure(e), Queue(tail, resultPool))))
              case q@Queue(_, _) =>
                LM.observerCpsMonad.flatMap(q.fsplit) {
                  case None => Queue(tail, resultPool).fsplit
                  case Some((tryHead, rest)) =>
                    LM.observerCpsMonad.pure(Some((tryHead, rest.merge(Queue(tail, resultPool)))))
                }
      }


      resultPool.findMaxPriority match {
        case Some(resultPoolPriority) =>
          asPQ.findMaxPriority(streams) match {
            case Some(streamsPriority) =>
              if (summon[Ordering[R]].gteq(resultPoolPriority, streamsPriority)) then
                getFromResultPool
              else if lsPolicy.maxSuboptimalResultPool > 0 && resultPool.size >= lsPolicy.maxSuboptimalResultPool then
                getFromResultPool
              else
                getFromStream(backupFromResultPool = true)
            case None =>
              getFromResultPool
          }
        case None =>
          getFromStream(backupFromResultPool = false)
      }
    }

    override def msplit: ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] = {
      val fStream = fsplit.map {
        singleton(_, R.one)
      }
      WaitF(fStream, R.one)
    }

    /**
     * Get only the first/best result without creating continuation structures.
     * Unlike fsplit, this doesn't merge remaining streams - it just discards them.
     */
    override def first: F[Option[Try[A]]] = {
      def getFromResultPool: F[Option[Try[A]]] = {
        resultPool.dequeue match
          case (Some(frs), _) =>
            summon[CpsTryMonad[F]].pure(Some(Success(frs.value)))
          case (None, _) =>
            summon[CpsTryMonad[F]].pure(None)
      }

      def getFromStream(backupFromResultPool: Boolean): F[Option[Try[A]]] = {
        val (optFrs, _) = asPQ.dequeue(streams)  // discard tail
        optFrs match
          case None =>
            if backupFromResultPool then getFromResultPool
            else summon[CpsTryMonad[F]].pure(None)
          case Some(frs) =>
            frs match
              case Empty() =>
                // Recursively try next element - but we've discarded the tail, so just return None
                summon[CpsTryMonad[F]].pure(None)
              case p@Pure(value, _) =>
                summon[CpsTryMonad[F]].pure(Some(Success(value)))
              case err@Error(e) =>
                summon[CpsTryMonad[F]].pure(Some(Failure(e)))
              case w@WaitF(waited, _) =>
                summon[CpsTryMonad[F]].flatMap(waited)(_.first)
              case s@Suspend(thunk, _) =>
                try thunk().first
                catch case NonFatal(e) => summon[CpsTryMonad[F]].pure(Some(Failure(e)))
              case q@Queue(_, _) =>
                q.first
      }

      resultPool.findMaxPriority match {
        case Some(resultPoolPriority) =>
          asPQ.findMaxPriority(streams) match {
            case Some(streamsPriority) =>
              if summon[Ordering[R]].gteq(resultPoolPriority, streamsPriority) then
                getFromResultPool
              else
                getFromStream(backupFromResultPool = true)
            case None =>
              getFromResultPool
          }
        case None =>
          getFromStream(backupFromResultPool = false)
      }
    }

  }

  object Queue {
    def empty[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy]: Queue[F, A, R] =
      Queue(asPQ.empty, asSSPQ.empty)
  }


  def empty[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy]: ScoredLogicStreamT[F, A, R] = Empty()

  def singleton[F[_] : CpsTryMonad, A, R: LinearlyOrderedGroup : LogicalSearchPolicy](value: A, priority: R): ScoredLogicStreamT[F, A, R] =
    Pure(value, priority)

  class ScoredLogicStreamTMonad[F[_] : CpsTryMonad, R: LinearlyOrderedGroup : LogicalSearchPolicy]
    extends CpsScoredLogicMonadInstanceContext[[A] =>> ScoredLogicStreamT[F, A, R], R] {

    override type Observer[A] = F[A]

    override val observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def pure[T](t: T): ScoredLogicStreamT[F, T, R] = ScoredLogicStreamT.Pure(t, summon[LinearlyOrderedMultiplicativeMonoid[R]].one)

    override def error[T](e: Throwable): ScoredLogicStreamT[F, T, R] = ScoredLogicStreamT.Error(e)

    override def mzero[T]: ScoredLogicStreamT[F, T, R] = ScoredLogicStreamT.empty

    override def scoredPure[A](a: A, score: R): ScoredLogicStreamT[F, A, R] = ScoredLogicStreamT.Pure(a, score)

    override def mplus[A](a: ScoredLogicStreamT[F, A, R], b: => ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] =
      a.scoredMplus(b, summon[LinearlyOrderedMultiplicativeMonoid[R]].one)

    override def scoredMplus[A](a: ScoredLogicStreamT[F, A, R], bScore: R, b: => ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, A, R] =
      a.scoredMplus(b, bScore)

    override def map[A, B](fa: ScoredLogicStreamT[F, A, R])(f: A => B): ScoredLogicStreamT[F, B, R] =
      fa.map(f)

    override def flatMap[A, B](fa: ScoredLogicStreamT[F, A, R])(f: A => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      fa.flatMap(f)

    override def flatMapTry[A, B](fa: ScoredLogicStreamT[F, A, R])(f: Try[A] => ScoredLogicStreamT[F, B, R]): ScoredLogicStreamT[F, B, R] =
      fa.flatMapTry(f)


    override def flattenObserver[A](fma: F[ScoredLogicStreamT[F, A, R]]): ScoredLogicStreamT[F, A, R] =
      ScoredLogicStreamT.WaitF(fma, summon[LinearlyOrderedMultiplicativeMonoid[R]].one)


    override def fsplit[A](c: ScoredLogicStreamT[F, A, R]): F[Option[(Try[A], ScoredLogicStreamT[F, A, R])]] =
      c.fsplit

    override def msplit[A](c: ScoredLogicStreamT[F, A, R]): ScoredLogicStreamT[F, Option[(Try[A], ScoredLogicStreamT[F, A, R])], R] =
      c.msplit

    override def first[A](fa: ScoredLogicStreamT[F, A, R]): F[Option[Try[A]]] =
      fa.first

  }


  given cpsScoredLogicMonad[F[_] : CpsTryMonad, R: LinearlyOrderedGroup : LogicalSearchPolicy]: ScoredLogicStreamTMonad[F, R] =
    new ScoredLogicStreamTMonad[F, R]


  private def LM[F[_] : CpsTryMonad, R: LinearlyOrderedGroup](using lm: ScoredLogicStreamTMonad[F, R]) = lm

  //def emptyQueue

}

