package cps.learning.examples.shortestPath

import scala.util.*
import cps.{learning, *}
import cps.syntax.*
import cps.learning.*
import cps.monads.logic.*
import cps.learning.examples.shortestPath.ShortestPath.SearchState

case class Edge[N](from: N, to: N, cost: Float)

trait GraphDB[N] {

  def neighbords(node: N): Seq[Edge[N]]

}

object ShortestPath {

  type Reached[N] = Map[N, (IndexedSeq[N], Float)]

  extension [N](m: Reached[N]) {

    def addEdge(e: Edge[N]): (Reached[N], Float) = {
      m.get(e.from) match
        case Some((path, prevCost)) =>
          val (newPath, newCost) = ((path.appended(e.to)), prevCost + e.cost)
          m.get(e.to) match
            case Some((prevPath, prevCost)) if (newCost > prevCost) => (m, prevCost)
            case _ => (m + (e.to -> (newPath, newCost)), newCost)
        case None =>
          throw new IllegalStateException(s"Incorrect state for adding edge ${e.from}->${e.to}, from is not in the reached map")
    }

    def distance(node: N) = m.get(node) match
      case Some((_, cost)) => cost
      case None => Float.MaxValue

  }

  case class SearchState[N](marks: Map[N, (IndexedSeq[N], Float)], visited: Set[N])

  def shortestPath0[F[_] : CpsScoredLogicMonad.Curry[Float], N](db: GraphDB[N], start: N, end: N): F[Option[IndexedSeq[N]]] = {

    //Wait for
    val F = summon[CpsScoredLogicMonad[F, Float]]

    def pathFrom(state: SearchState[N], current: N, currentCost: Float): F[SearchState[N]] = {
      F.multiScore(
        db.neighbords(current).map(e => (-(currentCost + e.cost) -> e)).toMap
          .map { case (score, e) => (score, () => F.pure(e)) }
      ).foldLeftWhileM(state)(state => !state.marks.contains(end)) {
        case (state, edge@Edge(_, to, _)) =>
          val (marks, toCost) = state.marks.addEdge(edge)
          val nextState = SearchState(marks, state.visited + to)
          if (state.visited.contains(to))
          then F.pure(nextState)
          else {
            pathFrom(nextState, to, toCost)
          }
      }

    }

    val state0 = SearchState(Map(start -> (IndexedSeq(start), 0.0f)), Set(start))
    summon[CpsScoredLogicMonad[F, Float]].map(pathFrom(state0, start, 0.0f)) {
      state =>
        state.marks.get(end) match {
          case Some((path, _)) => Some(path)
          case None => None
        }
    }
  }


  def shortestPath[F[_] : CpsOrderedLogicMonad.Curry[Float], N](db: GraphDB[N], start: N, end: N): F[Option[(IndexedSeq[N], Float)]] = {

    val F = summon[CpsOrderedLogicMonad[F, Float]]

    def fNeightbords(e: N): F[Edge[N]] = {
      F.order(
        F.fromCollection(db.neighbords(e))
      )(edge => -edge.cost)
    }

    def pathFrom(state: SearchState[N], nexts: F[Edge[N]]): F[SearchState[N]] = reify[F] {
      reflect(F.msplit(nexts)) match {
        case Some((Success(e), rest)) =>
          if state.visited.contains(e.to) then
            state
          else
            val (marks, newCost) = state.marks.addEdge(e)
            if newCost < state.marks.distance(e.to) then
              val nextState = SearchState(marks, state.visited + e.to)
              if e.to == end then
                nextState
              else
                val nextEdges = F.order(F.fromCollection(db.neighbords(e.to)))(e => -(newCost + e.cost))
                reflect(pathFrom(state, nextEdges |+| rest))
            else
              reflect(pathFrom(state, rest))
        case Some((Failure(e), rest)) =>
          throw e
        case None => state
      }
    }

    val state0 = SearchState(Map(start -> (IndexedSeq(start), 0.0f)), Set.empty)
    F.map(pathFrom(state0, F.fromCollection(db.neighbords(start)))) { state =>
      state.marks.get(end)
    }
  }


}

extension [F[_] : CpsScoredLogicMonad.Curry[Float], X](fx: F[X]) {

  def foldLeftWhileM[S](s: S)(cond: S => Boolean)(f: (S, X) => F[S]): F[S] = {
    if !cond(s) then summon[CpsScoredLogicMonad[F, Float]].pure(s)
    else
      summon[CpsScoredLogicMonad[F, Float]].msplit(fx).flatMap {
        case Some((Success(x), rest)) =>
          f(s, x).flatMap { newState =>
            rest.foldLeftWhileM(newState)(cond)(f)
          }
        case Some((Failure(e), rest)) =>
          summon[CpsScoredLogicMonad[F, Float]].error(e)
        case None => 
          summon[CpsScoredLogicMonad[F, Float]].pure(s)
      }
  }

}