package cps.rl.benchmarks

import scala.util.*
import scala.annotation.tailrec
import cps.*
import cps.monads.{*, given}
import cps.rl.*
import cps.rl.examples.shortestPath.{Edge, GraphDB}

/**
 * Iterative (stack-safe) implementation of Dijkstra's shortest path algorithm.
 * Uses a while loop with accumulator instead of monadic recursion to avoid
 * StackOverflowError on large graphs.
 */
object ShortestPathIterative {

  /**
   * Find shortest path using an iterative approach.
   *
   * @tparam F The stream monad type
   * @tparam O The observer monad type (how results are observed/evaluated)
   * @tparam N The node type
   */
  def shortestPath[F[_], O[_], N](db: GraphDB[N], start: N, end: N)(
    using F: CpsScoredLogicMonad[F, Float] { type Observer[X] = O[X] },
    O: CpsTryMonad[O]
  ): O[Option[IndexedSeq[N]]] = {
    import AdditiveScalingGroup.given

    if (start == end) {
      O.pure(Some(IndexedSeq(start)))
    } else {
      val neighbors = db.neighbords(start)
      if (neighbors.isEmpty) {
        O.pure(None)
      } else {
        runDijkstra(db, start, end, neighbors)
      }
    }
  }

  private def runDijkstra[F[_], O[_], N](
    db: GraphDB[N],
    start: N,
    end: N,
    initialNeighbors: Seq[Edge[N]]
  )(
    using F: CpsScoredLogicMonad[F, Float] { type Observer[X] = O[X] },
    O: CpsTryMonad[O]
  ): O[Option[IndexedSeq[N]]] = {
    import AdditiveScalingGroup.given

    case class Entry(node: N, path: IndexedSeq[N], cost: Float)

    def expand(e: Entry, settled: Set[N]): F[Entry] = {
      val neighbors = db.neighbords(e.node).filterNot(edge => settled.contains(edge.to))
      if (neighbors.isEmpty) F.mzero
      else F.multiScore(
        neighbors.map { edge =>
          val newCost = e.cost + edge.cost
          (-newCost, () => F.pure(Entry(edge.to, e.path :+ edge.to, newCost)))
        }
      )
    }

    // Build initial frontier
    var frontier: F[Entry] = F.multiScore(
      initialNeighbors.map { edge =>
        (-edge.cost, () => F.pure(Entry(edge.to, IndexedSeq(start, edge.to), edge.cost)))
      }
    )
    var settled: Set[N] = Set(start)
    var done = false
    var result: Option[IndexedSeq[N]] = None
    var error: Throwable = null

    // Iterative Dijkstra using first on msplit result
    while (!done) {
      val splitResult: O[Option[Try[Option[(Try[Entry], F[Entry])]]]] = F.first(F.msplit(frontier))

      O.flatMap(splitResult) {
        case Some(Success(None)) =>
          done = true
          result = None
          O.pure(())
        case Some(Success(Some((Success(entry), rest)))) =>
          if (settled.contains(entry.node)) {
            frontier = rest
          } else if (entry.node == end) {
            done = true
            result = Some(entry.path)
          } else {
            settled = settled + entry.node
            val expanded = expand(entry, settled)
            frontier = F.mplus(expanded, rest)
          }
          O.pure(())
        case Some(Success(Some((Failure(e), _)))) =>
          done = true
          error = e
          O.pure(())
        case Some(Failure(e)) =>
          done = true
          error = e
          O.pure(())
        case None =>
          done = true
          result = None
          O.pure(())
      }
    }

    if (error != null) O.error(error)
    else O.pure(result)
  }

  /**
   * Simplified version for CpsIdentity observer (synchronous evaluation).
   * This avoids the complexity of handling the observer monad in the loop.
   */
  def shortestPathSync[F[_], N](db: GraphDB[N], start: N, end: N)(
    using F: CpsScoredLogicMonad[F, Float] { type Observer[X] = X }
  ): Option[IndexedSeq[N]] = {
    import AdditiveScalingGroup.given

    if (start == end) {
      Some(IndexedSeq(start))
    } else {
      val neighbors = db.neighbords(start)
      if (neighbors.isEmpty) {
        None
      } else {
        runDijkstraSync(db, start, end, neighbors)
      }
    }
  }

  private def runDijkstraSync[F[_], N](
    db: GraphDB[N],
    start: N,
    end: N,
    initialNeighbors: Seq[Edge[N]]
  )(
    using F: CpsScoredLogicMonad[F, Float] { type Observer[X] = X }
  ): Option[IndexedSeq[N]] = {
    import AdditiveScalingGroup.given

    case class Entry(node: N, path: IndexedSeq[N], cost: Float)

    def expand(e: Entry, settled: Set[N]): F[Entry] = {
      val neighbors = db.neighbords(e.node).filterNot(edge => settled.contains(edge.to))
      if (neighbors.isEmpty) F.mzero
      else F.multiScore(
        neighbors.map { edge =>
          val newCost = e.cost + edge.cost
          (-newCost, () => F.pure(Entry(edge.to, e.path :+ edge.to, newCost)))
        }
      )
    }

    // Build initial frontier
    var frontier: F[Entry] = F.multiScore(
      initialNeighbors.map { edge =>
        (-edge.cost, () => F.pure(Entry(edge.to, IndexedSeq(start, edge.to), edge.cost)))
      }
    )
    var settled: Set[N] = Set(start)
    var done = false
    var result: Option[IndexedSeq[N]] = None

    // Iterative Dijkstra - since Observer[X] = X, first returns the value directly
    while (!done) {
      val splitResult: Option[Try[Option[(Try[Entry], F[Entry])]]] = F.first(F.msplit(frontier))

      splitResult match {
        case Some(Success(None)) =>
          done = true
          result = None
        case Some(Success(Some((Success(entry), rest)))) =>
          if (settled.contains(entry.node)) {
            frontier = rest
          } else if (entry.node == end) {
            done = true
            result = Some(entry.path)
          } else {
            settled = settled + entry.node
            val expanded = expand(entry, settled)
            frontier = F.mplus(expanded, rest)
          }
        case Some(Success(Some((Failure(e), _)))) =>
          throw e
        case Some(Failure(e)) =>
          throw e
        case None =>
          done = true
          result = None
      }
    }

    result
  }
}
