package cps.rl

import scala.util.Try
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import cps.*
import cps.syntax.{flatMap, map}
import cps.rl.ds.*
import cps.rl.ds.MinMax.given


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

  sealed trait Stream[F[_], A, R: ScalingGroup : Ordering : LogicalSearchPolicy](
    using val fm: CpsTryMonad[F],
    val sop: SuspendableObserverProvider[F]
  ) {

    def applyScore(r: R): Stream[F, A, R]

    def maxScore: R

    def |*|(r: R): Stream[F, A, R] = applyScore(r)

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R]

    def merge(other: Stream[F, A, R]): Stream[F, A, R]

    def map[B](f: A => B): Stream[F, B, R] = flatMap(a => Pure(f(a), one))

    def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R]

    def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R]

    /**
     * Stack-safe lazy version of fsplit.
     * Takes provider as parameter to ensure consistent path-dependent type.
     */
    def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]]

    /**
     * Split the stream into head and tail.
     * Delegates to lazyFsplit and runs through the stack-safe interpreter.
     */
    def fsplit: F[Option[(Try[A], Stream[F, A, R])]] = {
      val sp = sop
      sp.runSuspended(lazyFsplit(using sp))
    }

    def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R]

    /**
     * Stack-safe lazy version of first.
     * Defined via lazyFsplit - benchmarks showed no performance benefit from separate implementations.
     */
    def lazyFirst(using sp: SuspendableObserverProvider[F]): sp.SO[Option[Try[A]]] =
      sp.monad.map(lazyFsplit)(_.map(_._1))

    /**
     * Get the first/best result.
     */
    def first: F[Option[Try[A]]] = {
      val sp = sop
      sp.runSuspended(lazyFirst(using sp))
    }

    /**
     * Wrap this stream in a Suspend to defer its evaluation.
     * This provides trampolining to avoid stack overflow on deep recursion.
     */
    def suspended: Stream[F, A, R] = Suspend(() => this, summon[ScalingGroup[R]].one)

  }

  def R[R](using ScalingMonoid[R]) = summon[ScalingMonoid[R]]

  def rOrd[R](using Ordering[R]) = summon[Ordering[R]]

  def one[R](using ScalingMonoid[R]) = summon[ScalingMonoid[R]].one

  def lsPolicy[R](using lp: LogicalSearchPolicy[R]) = lp

  case class Empty[F[_]: CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]() extends Stream[F, A, R] {

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] =
      other.applyScore(otherScore)

    def merge(other: Stream[F, A, R]): Stream[F, A, R] = other

    def applyScore(r: R): Stream[F, A, R] = this

    def maxScore: R = summon[ScalingMonoid[R]].zero

    override def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] =
      Empty()

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] =
      Empty()

    override def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]] =
      sp.monad.pure(None)

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Pure(None, R.one)
    }

  }

  case class Pure[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](value: A, score: R) extends Stream[F, A, R] {

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
      // Wrap in Suspend to defer execution and avoid StackOverflow for deep flatMap chains
      Suspend(() => {
        try
          f(value) |*| score
        catch
          case NonFatal(e) => Error(e)
      }, score)
    }

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] = {
      // Wrap in Suspend to defer execution and avoid StackOverflow
      Suspend(() => {
        try
          f(scala.util.Success(value)) |*| score
        catch
          case NonFatal(e) => Error(e)
      }, score)
    }

    override def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]] =
      sp.monad.pure(Some((Success(value), Empty())))

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Pure(Some((Success(value), Empty())), R.one |*| score)
    }

  }

  case class Error[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](error: Throwable) extends Stream[F, A, R] {

    def applyScore(r: R): Stream[F, A, R] = this

    def maxScore: R = summon[ScalingMonoid[R]].maxPositiveValue

    def scoredMplus(other: => Stream[F, A, R], otherScore: R): Stream[F, A, R] =
      this

    def flatMap[B](f: A => Stream[F, B, R]): Stream[F, B, R] =
      this.asInstanceOf[Stream[F, B, R]]

    override def flatMapTry[B](f: Try[A] => Stream[F, B, R]): Stream[F, B, R] = {
      // Wrap in Suspend to defer execution and avoid StackOverflow
      Suspend(() => {
        try
          f(Failure(error))
        catch
          case NonFatal(e) => Error(e)
      }, maxScore)
    }

    override def merge(other: Stream[F, A, R]): Stream[F, A, R] =
      this

    override def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]] =
      sp.monad.pure(Some((Failure(error), Empty())))

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Pure(Some((Failure(error), Empty())), R.one |*| maxScore)
    }

  }

  case class WaitF[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](waited: F[Stream[F, A, R]], score: R) extends Stream[F, A, R] {

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

    override def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]] =
      // Use flatMap to compose lazyFsplit calls instead of calling .fsplit which triggers runSuspended recursively
      sp.monad.flatMap(sp.suspend(waited))(_.lazyFsplit)

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      WaitF(summon[CpsTryMonad[F]].map(waited) { s =>
        s.msplit
      }, score)
    }

  }

  case class Suspend[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](
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

    override def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]] =
      sp.monad.flatDelay {
        try {
          thunk().lazyFsplit
        } catch {
          case NonFatal(e) => sp.monad.pure(Some((Failure(e), Empty())))
        }
      }

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      Suspend(() => thunk().msplit, score)
    }

  }


  case class Queue[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](streams: PQ[Stream[F, A, R], R], resultPool: SPQ[Pure[F, A, R], R]) extends Stream[F, A, R] {

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

    override def lazyFsplit(using sp: SuspendableObserverProvider[F]): sp.SO[Option[(Try[A], Stream[F, A, R])]] = {
      val SM = sp.monad
      val FM = summon[CpsTryMonad[F]]

      def getFromResultPool(currentStreams: PQ[Stream[F, A, R], R]): sp.SO[Option[(Try[A], Stream[F, A, R])]] = {
        asSSPQ.dequeue(resultPool) match
          case (Some(frs), tailPool) =>
            SM.pure(Some((Success(frs.value), Queue(currentStreams, tailPool))))
          case (None, _) =>
            SM.pure(None)
      }

      // Iterative version to avoid stack overflow on Empty streams
      def getFromStream(initialStreams: PQ[Stream[F, A, R], R], backupFromResultPool: Boolean): sp.SO[Option[(Try[A], Stream[F, A, R])]] = {
        var currentStreams = initialStreams

        // Loop to skip over Empty streams without recursion
        while (true) {
          val (optFrs, tail) = asPQ.dequeue(currentStreams)
          optFrs match
            case None =>
              if backupFromResultPool then
                return getFromResultPool(tail)
              else
                return SM.pure(None)
            case Some(frs) =>
              frs match
                case Empty() =>
                  // Instead of recursive call, continue loop with tail
                  currentStreams = tail
                case p@Pure(value, score) =>
                  return SM.pure(Some((Success(value), Queue(tail, resultPool))))
                case err@Error(e) =>
                  return SM.pure(Some((Failure(e), Queue(tail, resultPool))))
                case w@WaitF(waited, score) =>
                  val restQueue = Queue(tail, resultPool)
                  return sp.suspend {
                    FM.flatMap(waited) { s =>
                      FM.flatMap(s.fsplit) {
                        case None => restQueue.fsplit
                        case Some((tryHead, rest)) =>
                          FM.pure(Some((tryHead, rest.merge(restQueue))))
                      }
                    }
                  }
                case s@Suspend(thunk, score) =>
                  val restQueue = Queue(tail, resultPool)
                  return SM.flatDelay {
                    try {
                      SM.flatMap((thunk() |*| score).lazyFsplit) {
                        case None => restQueue.lazyFsplit
                        case Some((tryHead, rest)) =>
                          SM.pure(Some((tryHead, rest.merge(restQueue))))
                      }
                    } catch {
                      case NonFatal(e) =>
                        SM.pure(Some((Failure(e), restQueue)))
                    }
                  }
                case q@Queue(_, _) =>
                  val restQueue = Queue(tail, resultPool)
                  return SM.flatDelay {
                    SM.flatMap(q.lazyFsplit) {
                      case None => restQueue.lazyFsplit
                      case Some((tryHead, rest)) =>
                        SM.pure(Some((tryHead, rest.merge(restQueue))))
                    }
                  }
        }
        // Unreachable, but needed for type checker
        SM.pure(None)
      }


      asSSPQ.findMaxPriority(resultPool) match {
        case Some(resultPoolPriority) =>
          asPQ.findMaxPriority(streams) match {
            case Some(streamsPriority) =>
              if (summon[Ordering[R]].gteq(resultPoolPriority, streamsPriority)) then
                getFromResultPool(streams)
              else if lsPolicy.maxSuboptimalResultPool > 0 && asSSPQ.size(resultPool) >= lsPolicy.maxSuboptimalResultPool then
                getFromResultPool(streams)
              else
                getFromStream(streams, backupFromResultPool = true)
            case None =>
              getFromResultPool(streams)
          }
        case None =>
          getFromStream(streams, backupFromResultPool = false)
      }
    }

    override def msplit: Stream[F, Option[(Try[A], Stream[F, A, R])], R] = {
      val fStream = fsplit.map {
        singleton(_, R.one)
      }
      WaitF(fStream, R.one)
    }

  }

  object Queue {
    def empty[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]: Queue[F, A, R] =
      Queue(asPQ.empty, asSSPQ.empty)
  }


  def empty[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]: Stream[F, A, R] = Empty()

  def singleton[F[_] : CpsTryMonad : SuspendableObserverProvider, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](value: A, priority: R): Stream[F, A, R] =
    Pure(value, priority)

  class StreamMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy](
    using val soProvider: SuspendableObserverProvider[F]
  ) extends CpsScoredLogicMonadInstanceContext[[A] =>> Stream[F, A, R], R] {

    override type Observer[A] = F[A]

    override val observerCpsMonad: CpsTryMonad[F] = summon[CpsTryMonad[F]]

    override type SuspendableObserver[X] = soProvider.SO[X]

    override def suspendableMonad: CpsTryEffectMonad[SuspendableObserver] =
      soProvider.monad

    override def runSuspended[A](sa: soProvider.SO[A]): F[A] =
      soProvider.runSuspended(sa)

    override def suspendInObserver[A](oa: => F[A]): soProvider.SO[A] =
      soProvider.suspend(oa)

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


  def cpsScoredLogicMonad[F[_] : CpsTryMonad : SuspendableObserverProvider, R: ScalingGroup : Ordering : LogicalSearchPolicy]: StreamMonad[F, R] =
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
