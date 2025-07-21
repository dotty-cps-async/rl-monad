package cps.learning.examples.shortestPath

import cps.learning.CpsScoredLogicMonad
import cps.learning.examples.shortestPath.ShortestPath.SearchState

case class Edge[N](from: N, to: N, cost: Float)

trait GraphDB[N] {
  
  def neighbords(node: N): Seq[Edge]
  
}

object ShortestPath {

  type Reached[N] = Map[N,(IndexedSeq[N],Float)])

  extension [N](m: Reached[N]) {

    def addEdge(e: Edge): Map[N, IndexedSeq[N]] = {
      marks.get(e.from) match
        case Some((path, prevCost)) =>
          val (newPath, newCost) = ((path +: e.to), prevCost + e.cost)
          market.get(to) match
            case Some((prevPath, prevCost)) if (newCost > prevCost) =>
              (marks, prevCost)
            case _ =>
              (marks + (e.to -> (newPath, newCost)), newCost)
        case None =>
          throw new IllegalStateException(s"Incorrect state for adding edge ${from}->${to}")
    }

    def distance(n:Node) = m.get(node) match
      case Some((_, cost)) => cost
      case None => Float.MaxValue

  }
  
  case class SearchState(marks: Map[N,(IndexedSeq[N], Float)], visited: Set[N])

  def shortestPath0[F[_] : CpsScoredLogicMonad, N](db: GraphDB, start: N, end: N): F[()] = {


    def pathFrom(state: SearchState, current: N, currentCost:Float): F[State] = {
      F.multiScore(
        db.neighbords(current).map(e => (e->currentCost + e.cost)).toMap
      ).foldLeftWhileM(state)(state => !state.marks.contains(end) ) {
        case (state, edge@Edge(_.to, _)) =>
            val marks = state.addEdge(e)
            val nextState = SearchState(marks, state.visited + to)
            if (state.visited.contains(to))
              then F.pure(nextState)
            else {
              pathFrom(nextStat)
            }
      }


      nextNodes.headOption match
        case None =>
          F.pure(state)
        case Some(e@Edge(current, to, _)) =>
          val marks = state.addEdge(e)
          val nextState = SearchState(marks, state.visited + to)
          if (state.visited.contains(to))
          then F.pure(nextState)
          else {
            (pathFrom(nextState, db.neighbours(to) -> nextState.distance(to)),
              |+|
                pathFrom (nextState, nextNodes.tail) -> nextState.distance(current)
            )
          }
    }

    val state0 = State(Map(start -> (IndexedSeq[Start], 0.0f)), Set(start))
    pathFrom(state0, start, db.neighbours(start))
  }


  def shortestPath[F[_]:CpsOrderedLogicMonad, N](db: GraphDB, start: N, end: N): F[Option[(Seq[Node],Float)]] = {

    def pathFrom(state: Map[N,(Seq[N],Float)] , nexts:F[Edge]): F[State] = {
      nexts.msplit.flatMap {
        case Some((e, rest)) =>
          if state.distance(e.from) + e.cost < state.distance(e.to) then
            val nextState = state.addEdge(e)
            if (e.to == end) then
              F.pure(nextState)
            else
              val nextEdges = db.neighbours(e.to).map(e => (e, -(currentCost + e.cost))).toMap
              pathFrom(state, F.multiScore(nextEdges) |+| rest)
          else
           pathForm(state, rest)
        case None => F.pure(state)
      }
    }

    val state0 = Map(start -> (IndexedSeq[Start], 0.0f))
    pathFrom(state0, start, db.neighbours(start).map(e => (e, -e.cost)).toMap).map(_.get(end))
  }

  (a -> scoreA) |+| (b -> scoreB)

  scoredMplus(a,scoreA,b,scoreB)


    val state0 = SearchState(Map(start -> (IndexedSeq[Start], 0.0f)), Set(start), start, false)
    pathFrom(state0)
  }

  
}
