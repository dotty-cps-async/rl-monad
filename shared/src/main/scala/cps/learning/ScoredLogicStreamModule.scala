package cps.learning

import scala.util.Try
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import cps.*
import cps.syntax.{flatMap, map}
import cps.learning.ds.*
import cps.learning.ds.MinMax.given


/**
 * Abstract module for ScoredLogicStream that can be parameterized by different queue implementations.
 * This allows benchmarking different heap implementations (PairingHeap, FingerTree, etc.)
 * without code duplication.
 *
 * @tparam PQ The priority queue type constructor for streams
 * @tparam SPQ The sized priority queue type constructor for result pool
 */
abstract class ScoredLogicStreamModule[
  PQ[_, _],
  SPQ[_, _]
] {

  def asPQ[R: ScalingGroup : Ordering]: AsScaledPriorityQueue[PQ, R]

  def asSSPQ[R: ScalingGroup : Ordering]: AsSizedScaledPriorityQueue[SPQ, R]

  sealed trait Stream[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy] {

    def applyScore(r: R): Stream[F, A, R]

    def maxScore: R

    def |*|(r: R): Stream[F, A, R] = applyScore(r)

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R]

    def merge(other: Stream[F, A, R]): Stream[F, A, R]

    def map[B](f: A => B): Stream[F, B, R] = flatMap(a => Pure(f(a), one))

    def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R]

    def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R]

    def fsplit: F[Option[(Try[A], Stream[F, A, R])]]

    def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R]

    def first: F[Option[Try[A]]]

  }

  def R[R](using ScalingMonoid[R]) = summon[ScalingMonoid[R]]

  def rOrd[R](using Ordering[R]) = summon[Ordering[R]]

  def one[R](using ScalingMonoid[R]) = summon[ScalingMonoid[R]].one

  def lsPolicy[R](using lp: LogicalSearchPolicy[R]) = lp

  case class Empty[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]() extends Stream[F, A, R] {

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] =
      other.applyScore(otherScore)

    def merge(other: Stream[F, A, R]): Stream[F, A, R] = other

    def applyScore(r: R): Stream[F, A, R] = this

    def maxScore: R = summon[ScalingMonoid[R]].zero

    override def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] =
      Empty()

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] =
      Empty()

    override def fsplit: F[Option[(Try[A], Stream[F, A, R])]] =
      summon[CpsTryMonad[F]].pure(None)

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Pure(None, R.one)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].pure(None)

  }

  case class Pure[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](value: A, score: R) extends Stream[F, A, R] {

    def applyScore(r: R): Stream[F, A, R] =
      Pure(value, summon[ScalingGroup[R]].scaleBy(r, score))

    def maxScore: R = score

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] = {
      Queue.empty.enqueue(this, score).enqueue(Suspend(() => other, otherScore), otherScore)
    }

    override def merge(other: Stream[F, A, R]): Stream[F, A, R] = {
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

    override def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] = {
      try
        f(value) |*| score
      catch
        case NonFatal(e) => Error(e)
    }

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] = {
      try
        f(scala.util.Success(value)) |*| score
      catch
        case NonFatal(e) => Error(e)
    }

    override def fsplit: F[Option[(Try[A], Stream[F, A, R])]] =
      summon[CpsTryMonad[F]].pure(Some((Success(value), Empty())))

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Pure(Some((Success(value), Empty())), R.one |*| score)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].pure(Some(Success(value)))

  }

  case class Error[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](error: Throwable) extends Stream[F, A, R] {

    def applyScore(r: R): Stream[F, A, R] = this

    def maxScore: R = summon[ScalingMonoid[R]].maxPositiveValue

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] =
      this

    def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] =
      this.asInstanceOf[Stream[F, B, R]]

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] = {
      try
        f(Failure(error))
      catch
        case NonFatal(e) => Error(e)
    }

    override def merge(other: Stream[F, A, R]): Stream[F, A, R] =
      this

    override def fsplit: F[Option[(Try[A], Stream[F, A, R])]] =
      summon[CpsTryMonad[F]].pure(Some((Failure(error), Empty())))

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Pure(Some((Failure(error), Empty())), R.one |*| maxScore)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].pure(Some(Failure(error)))

  }

  case class WaitF[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](waited: F[Stream[F, A, R]], score: R) extends Stream[F, A, R] {

    def applyScore(r: R): Stream[F, A, R] =
      WaitF(waited, summon[ScalingGroup[R]].scaleBy(r, score))

    def maxScore: R = score

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] = {
      Queue.empty.enqueue(this, score).enqueue(Suspend(() => other, otherScore), otherScore)
    }

    override def merge(other: Stream[F, A, R]): Stream[F, A, R] = {
      WaitF(summon[CpsTryMonad[F]].map(waited)(x => (x |*| score).merge(other)), rOrd.max(score, other.maxScore))
    }

    override def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] =
      WaitF(summon[CpsTryMonad[F]].flatMap(waited)(s => summon[CpsTryMonad[F]].pure(s.flatMap(f))), score)

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] =
      WaitF(summon[CpsTryMonad[F]].flatMap(waited)(s => summon[CpsTryMonad[F]].pure(s.flatMapTry(f))), score)

    override def fsplit: F[Option[(Try[A], Stream[F, A, R])]] =
      summon[CpsTryMonad[F]].flatMap(waited) { s =>
        summon[CpsTryMonad[F]].map(s.fsplit) {
          case None => None
          case Some((tryHead, rest)) =>
            Some((tryHead, rest))
        }
      }

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      WaitF(summon[CpsTryMonad[F]].map(waited) { s =>
        s.msplit
      }, score)
    }

    override def first: F[Option[Try[A]]] =
      summon[CpsTryMonad[F]].flatMap(waited)(_.first)

  }

  case class Suspend[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](
                                                                                            thunk: () => Stream[F, A, R], score: R) extends Stream[F, A, R] {

    def applyScore(r: R): Stream[F, A, R] =
      Suspend(thunk, summon[ScalingGroup[R]].scaleBy(r, score))

    def maxScore: R = score

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] = {
      Queue.empty.enqueue(this, score).enqueue(Suspend(() => other, otherScore), otherScore)
    }

    override def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] =
      Suspend(() => thunk().flatMap(f), score)

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] =
      Suspend(() => thunk().flatMapTry(f), score)

    override def merge(other: Stream[F, A, R]): Stream[F, A, R] = {
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

    override def fsplit: F[Option[(Try[A], Stream[F, A, R])]] =
      try {
        thunk().fsplit
      } catch {
        case NonFatal(e) =>
          summon[CpsTryMonad[F]].pure(Some((Failure(e), Empty())))
      }

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
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


  case class Queue[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](streams: PQ[Stream[F, A, R], R], resultPool: SPQ[Pure[F, A, R], R]) extends Stream[F, A, R] {

    def enqueue(other: Stream[F, A, R], otherScore: R): Queue[F, A, R] = {
      Queue(asPQ.enqueue(other, otherScore, streams), resultPool)
    }

    def applyScore(r: R): Stream[F, A, R] = {
      Queue(asPQ.scale(streams, r), asSSPQ.scale(resultPool, r))
    }

    def dequeue: Option[(Stream[F, A, R], Queue[F, A, R])] = {
      asSSPQ.findMaxPriority(resultPool) match {
        case Some(rpPrirority) =>
          asPQ.findMaxPriority(streams) match
            case Some(streamsPriority) =>
              if (summon[Ordering[R]].gteq(rpPrirority, streamsPriority)) then
                val (Some(frs), tailPool) = asSSPQ.dequeue(resultPool): @unchecked
                Some(frs, Queue(streams, tailPool))
              else
                val (Some(frs), tail) = asPQ.dequeue(streams): @unchecked
                Some((frs, Queue(tail, resultPool)))
            case None =>
              val (Some(frs), tailPool) = asSSPQ.dequeue(resultPool): @unchecked
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
          asSSPQ.findMaxPriority(resultPool) match {
            case None => R.zero
            case Some(rpMaxPriority) => rpMaxPriority
          }
        case Some(maxPriority) =>
          asSSPQ.findMaxPriority(resultPool) match {
            case None => maxPriority
            case Some(rpMaxPriority) => rOrd.max(maxPriority, rpMaxPriority)
          }
    }

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] = {
      asPQ.findMaxPriority(streams) match
        case None =>
          Suspend(() => other, otherScore)
        case Some(maxPriority) =>
          val otherSuspended = Suspend(() => other, otherScore)
          Queue(asPQ.enqueue(otherSuspended, otherScore, streams), asSSPQ.empty)
    }

    override def merge(other: Stream[F, A, R]): Stream[F, A, R] = {
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

    override def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] = {
      dequeue match
        case None => Empty()
        case Some((frs, tail)) =>
          // Keep tail lazy to avoid forcing all queued branches at once
          frs.flatMap(f).merge(Suspend(() => tail.flatMap(f), tail.maxScore))
    }

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] = {
      dequeue match {
        case None => Empty()
        case Some((frs, tail)) =>
          // Keep tail lazy to avoid forcing all queued branches at once
          frs.flatMapTry(f).merge(Suspend(() => tail.flatMapTry(f), tail.maxScore))
      }
    }

    override def fsplit: F[Option[(Try[A], Stream[F, A, R])]] = {

      def getFromResultPool: F[Option[(Try[A], Stream[F, A, R])]] = {
        asSSPQ.dequeue(resultPool) match
          case (Some(frs), tailPool) =>
            summon[CpsTryMonad[F]].pure(Some((Success(frs.value), Queue(streams, tailPool))))
          case (None, _) =>
            summon[CpsTryMonad[F]].pure(None)
      }

      def getFromStream(backupFromResultPool: Boolean): F[Option[(Try[A], Stream[F, A, R])]] = {
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
                  (thunk() |*| score).fsplit.flatMap {  // Apply stored score when expanding
                    case None => Queue(tail, resultPool).fsplit
                    case Some((tryHead, rest)) =>
                      summon[CpsTryMonad[F]].pure(Some((tryHead, rest.merge(Queue(tail, resultPool)))))
                  }
                catch
                  case NonFatal(e) =>
                    summon[CpsTryMonad[F]].pure(Some((Failure(e), Queue(tail, resultPool))))
              case q@Queue(_, _) =>
                summon[CpsTryMonad[F]].flatMap(q.fsplit) {
                  case None => Queue(tail, resultPool).fsplit
                  case Some((tryHead, rest)) =>
                    summon[CpsTryMonad[F]].pure(Some((tryHead, rest.merge(Queue(tail, resultPool)))))
                }
      }


      asSSPQ.findMaxPriority(resultPool) match {
        case Some(resultPoolPriority) =>
          asPQ.findMaxPriority(streams) match {
            case Some(streamsPriority) =>
              if (summon[Ordering[R]].gteq(resultPoolPriority, streamsPriority)) then
                getFromResultPool
              else if lsPolicy.maxSuboptimalResultPool > 0 && asSSPQ.size(resultPool) >= lsPolicy.maxSuboptimalResultPool then
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

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      val fStream = fsplit.map {
        singleton(_, R.one)
      }
      WaitF(fStream, R.one)
    }

    override def first: F[Option[Try[A]]] = {
      def getFromResultPool: F[Option[Try[A]]] = {
        asSSPQ.dequeue(resultPool) match
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
              case s@Suspend(thunk, score) =>
                try (thunk() |*| score).first  // Apply stored score when expanding
                catch case NonFatal(e) => summon[CpsTryMonad[F]].pure(Some(Failure(e)))
              case q@Queue(_, _) =>
                q.first
      }

      asSSPQ.findMaxPriority(resultPool) match {
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
    def empty[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]: Queue[F, A, R] =
      Queue(asPQ.empty, asSSPQ.empty)
  }


  def empty[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]: Stream[F, A, R] = Empty()

  def singleton[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](value: A, priority: R): Stream[F, A, R] =
    Pure(value, priority)

  class StreamMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy]
    extends CpsScoredLogicMonadInstanceContext[[A] =>> Stream[F, A, R], R] {

    override type Observer[A] = F[A]

    override val observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override def pure[T](t: T): Stream[F, T, R] = Pure(t, summon[ScalingMonoid[R]].one)

    override def error[T](e: Throwable): Stream[F, T, R] = Error(e)

    override def mzero[T]: Stream[F, T, R] = Empty()

    override def scoredPure[A](a: A, score: R): Stream[F, A, R] = Pure(a, score)

    override def mplus[A](a: Stream[F, A, R], b: => Stream[F, A, R]): Stream[F, A, R] =
      a.merge(b)

    override def scoredMplus[A](a: Stream[F, A, R], bScore: R, b: => Stream[F, A, R]): Stream[F, A, R] =
      a.scoredMplus(b, bScore)

    override def map[A, B](fa: Stream[F, A, R])(f: A => B): Stream[F, B, R] =
      fa.map(f)

    override def flatMap[A, B](fa: Stream[F, A, R])(f: A => Stream[F, B, R]): Stream[F, B, R] =
      fa.flatMap(f)

    override def flatMapTry[A, B](fa: Stream[F, A, R])(f: Try[A] => Stream[F, B, R]): Stream[F, B, R] =
      fa.flatMapTry(f)


    override def flattenObserver[A](fma: F[Stream[F, A, R]]): Stream[F, A, R] =
      WaitF(fma, summon[ScalingMonoid[R]].one)


    override def fsplit[A](c: Stream[F, A, R]): F[Option[(Try[A], Stream[F, A, R])]] =
      c.fsplit

    override def msplit[A](c: Stream[F, A, R]): Stream[F, Option[(Try[A], Stream[F, A, R])], R] =
      c.msplit

    override def first[A](fa: Stream[F, A, R]): F[Option[Try[A]]] =
      fa.first

  }


  def cpsScoredLogicMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy]: StreamMonad[F, R] =
    new StreamMonad[F, R]


  private def LM[F[_] : CpsTryMonad, R: ScalingGroup : Ordering](using lm: StreamMonad[F, R]): StreamMonad[F, R] = lm

}


// Type aliases for priority queue types
type PairingHeapPQ[A, R] = ScaledBootstrappedPairingHeap[(A, R), R]
type PairingHeapSizedPQ[A, R] = ScaledPriorityQueueWithSize[PairingHeapPQ, A, R]

type SimplePairingHeapPQ[A, R] = ScaledPairingHeap[(A, R), R]
type SimplePairingHeapSizedPQ[A, R] = ScaledPriorityQueueWithSize[SimplePairingHeapPQ, A, R]

type FingerTreePQ[A, R] = ScaledMaxFingerTree[A, R]
type FingerTreeSizedPQ[A, R] = ScaledPriorityQueueWithSize[FingerTreePQ, A, R]


/**
 * Concrete module using PairingHeap-based queues (the default implementation)
 */
object PairingHeapStreamModule extends ScoredLogicStreamModule[
  PairingHeapPQ,
  PairingHeapSizedPQ
] {

  override def asPQ[R: ScalingGroup : Ordering]: AsScaledPriorityQueue[PairingHeapPQ, R] =
    summon[AsScaledPriorityQueue[PairingHeapPQ, R]]

  override def asSSPQ[R: ScalingGroup : Ordering]: AsSizedScaledPriorityQueue[PairingHeapSizedPQ, R] =
    summon[AsSizedScaledPriorityQueue[PairingHeapSizedPQ, R]]

  given streamMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy]: StreamMonad[F, R] =
    cpsScoredLogicMonad[F, R]

}


/**
 * Concrete module using FingerTree-based queues
 */
object FingerTreeStreamModule extends ScoredLogicStreamModule[
  FingerTreePQ,
  FingerTreeSizedPQ
] {

  override def asPQ[R: ScalingGroup : Ordering]: AsScaledPriorityQueue[FingerTreePQ, R] =
    summon[AsScaledPriorityQueue[FingerTreePQ, R]]

  override def asSSPQ[R: ScalingGroup : Ordering]: AsSizedScaledPriorityQueue[FingerTreeSizedPQ, R] =
    summon[AsSizedScaledPriorityQueue[FingerTreeSizedPQ, R]]

  given streamMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy]: StreamMonad[F, R] =
    cpsScoredLogicMonad[F, R]

}


/**
 * Concrete module using simple (non-bootstrapped) PairingHeap-based queues.
 * Provided for comparison with the bootstrapped version.
 */
object SimplePairingHeapStreamModule extends ScoredLogicStreamModule[
  SimplePairingHeapPQ,
  SimplePairingHeapSizedPQ
] {

  override def asPQ[R: ScalingGroup : Ordering]: AsScaledPriorityQueue[SimplePairingHeapPQ, R] =
    summon[AsScaledPriorityQueue[SimplePairingHeapPQ, R]]

  override def asSSPQ[R: ScalingGroup : Ordering]: AsSizedScaledPriorityQueue[SimplePairingHeapSizedPQ, R] =
    summon[AsSizedScaledPriorityQueue[SimplePairingHeapSizedPQ, R]]

  given streamMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy]: StreamMonad[F, R] =
    cpsScoredLogicMonad[F, R]

}
