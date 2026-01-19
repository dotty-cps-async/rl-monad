package cps.rl.examples.shortestPath

import scala.util.*
import cps.{rl,*}
import cps.syntax.*
import cps.rl.*
import cps.monads.logic.*

case class Edge[N](from: N, to: N, cost: Float)

trait GraphDB[N] {

  def neighbords(node: N): Seq[Edge[N]]

}

object ShortestPath {

  /**
   * Find shortest path using Dijkstra's algorithm with the scored logic monad.
   *
   * Uses AdditiveScalingGroup where |*| is addition, not multiplication.
   * This allows using negative costs directly as scores:
   *   -cost1 + -cost2 = -(cost1 + cost2)
   *
   * Higher score (less negative) = lower cost = higher priority.
   *
   * This is a lazy algorithm - paths are explored on demand via the
   * monad's priority queue. The thunks in multiScore defer computation
   * until msplit requests the next best element.
   */
  def shortestPath[F[_] : CpsScoredLogicMonad.Curry[Float], N](db: GraphDB[N], start: N, end: N): F[Option[IndexedSeq[N]]] = {
    import AdditiveScalingGroup.given
    val F = summon[CpsScoredLogicMonad[F, Float]]

    if (start == end) {
      return F.pure(Some(IndexedSeq(start)))
    }

    case class Entry(node: N, path: IndexedSeq[N], cost: Float)

    /**
     * Lazily expand an entry to neighbor entries.
     * Each neighbor is scored by negative total cost.
     * Thunks defer computation until the monad requests them.
     */
    def expand(e: Entry, settled: Set[N]): F[Entry] = {
      val neighbors = db.neighbords(e.node).filterNot(edge => settled.contains(edge.to))
      if (neighbors.isEmpty) F.mzero
      else F.multiScore(
        neighbors.map { edge =>
          val newCost = e.cost + edge.cost
          // Score = -totalCost (higher = better = lower cost)
          (-newCost, () => F.pure(Entry(edge.to, e.path :+ edge.to, newCost)))
        }
      )
    }

    /**
     * Dijkstra's algorithm using the monad's priority queue.
     * msplit returns the best (highest score = lowest cost) entry.
     */
    def dijkstra(frontier: F[Entry], settled: Set[N]): F[Option[IndexedSeq[N]]] = {
      F.flatMap(F.msplit(frontier)) {
        case None => F.pure(None)
        case Some((scala.util.Failure(e), _)) => F.error(e)
        case Some((scala.util.Success(entry), rest)) =>
          if (settled.contains(entry.node)) {
            dijkstra(rest, settled)
          } else if (entry.node == end) {
            F.pure(Some(entry.path))
          } else {
            val newSettled = settled + entry.node
            val expanded = expand(entry, newSettled)
            // mplus merges preserving internal priorities
            val newFrontier = F.mplus(expanded, rest)
            dijkstra(newFrontier, newSettled)
          }
      }
    }

    val neighbors = db.neighbords(start)
    if (neighbors.isEmpty) {
      F.pure(None)
    } else {
      val initialFrontier: F[Entry] = F.multiScore(
        neighbors.map { edge =>
          (-edge.cost, () => F.pure(Entry(edge.to, IndexedSeq(start, edge.to), edge.cost)))
        }
      )
      dijkstra(initialFrontier, Set(start))
    }
  }

}
