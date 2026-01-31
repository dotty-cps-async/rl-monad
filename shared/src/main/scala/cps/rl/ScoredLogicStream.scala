package cps.rl

import scala.util.Try
import cps.*
import cps.monads.logic.SuspendableObserverProvider
import cps.rl.ds.*

/**
 * ScoredLogicStreamT is a type alias to the default module's Stream type.
 * Uses PairingHeap-based priority queues by default.
 *
 * For alternative implementations (e.g., FingerTree), use the modules directly:
 *   - PairingHeapStreamModule.Stream
 *   - FingerTreeStreamModule.Stream
 */
type ScoredLogicStreamT[F[_], A, R] = PairingHeapStreamModule.Stream[F, A, R]

object ScoredLogicStreamT {

  // Re-export type aliases for backward compatibility
  type DefaultQueue[A, R] = PairingHeapPQ[A, R]
  type DefaultSizedQueue[A, R] = PairingHeapSizedPQ[A, R]

  // Re-export case class constructors via the module
  def Empty[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](): ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.Empty()

  def Pure[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](value: A, score: R): ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.Pure(value, score)

  def Error[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](error: Throwable): ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.Error(error)

  def WaitF[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](waited: F[ScoredLogicStreamT[F, A, R]], score: R): ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.WaitF(waited, score)

  def Suspend[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](thunk: () => ScoredLogicStreamT[F, A, R], score: R): ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.Suspend(thunk, score)

  // Helper methods
  def empty[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy]: ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.empty

  def singleton[F[_] : CpsTryMonad, A, R: ScalingGroup : Ordering : LogicalSearchPolicy](value: A, priority: R): ScoredLogicStreamT[F, A, R] =
    PairingHeapStreamModule.singleton(value, priority)

  // Re-export the monad type alias
  type ScoredLogicStreamTMonad[F[_], R] = PairingHeapStreamModule.StreamMonad[F, R]

  // Re-export the given instance for backward compatibility
  given cpsScoredLogicMonad[F[_] : CpsTryMonad, R: ScalingGroup : Ordering : LogicalSearchPolicy]: ScoredLogicStreamTMonad[F, R] =
    PairingHeapStreamModule.cpsScoredLogicMonad[F, R]

}
