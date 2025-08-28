package cps.learning

import scala.annotation.tailrec

import cps.*
import cps.monads.{*, given}
import cps.monads.logic.*
import cps.learning.ds.*

type OrderedLogicStream[A, R] = OrderedLogicStreamT[ScaledPairingHeap, CpsIdentity, A, R]

class OrderedLogicStreamT[H[_, _] : AsScaledPriorityQueue.Curry[R], F[_], A, R: Ordering](score: A => R, windowSize: Int,
                                                                                          priorityQueue: H[A, R],
                                                                                          rest: LogicStreamT[F, A]) {


  def asScaledPriorityQueueHR: AsScaledPriorityQueue[H, R] = summon[AsScaledPriorityQueue[H, R]]


  def order(newScore: A => R, newWindowSize: Int = windowSize): OrderedLogicStreamT[H, F, A, R] = {
    new OrderedLogicStreamT(newScore, newWindowSize, rebuildQueue(newScore), rest)
  }


  private def rebuildQueue(newScore: A => R): H[A, R] = {
    // Rebuild the priority queue based on the new score function


    @tailrec
    def go(oldQueue: H[A, R], nextQueue: H[A, R]): H[A, R] = {
      asScaledPriorityQueueHR.dequeue(oldQueue) match {
        case (None, _) => nextQueue
        case (Some(a), remaining) =>
          val newScoreValue = newScore(a)
          val updatedQueue = asScaledPriorityQueueHR.enqueue(a, newScoreValue, nextQueue)
          go(remaining, updatedQueue)
      }
    }

    go(priorityQueue, asScaledPriorityQueueHR.empty)

  }

}

