package cps.learning.examples.shortestPath

import munit.*
import scala.util.Success
import cps.*
import cps.monads.{*, given}
import cps.learning.*
import cps.learning.ScoredLogicStreamT.given
import cps.learning.ScalingRing.given
import cps.learning.ds.LogicalSearchPolicy.given

class ShortestPathSuite extends FunSuite {

  type ScoredStream[A] = ScoredLogicStreamT[CpsIdentity, A, Float]

  class SimpleGraphDB(edges: Map[String, Seq[Edge[String]]]) extends GraphDB[String] {
    def neighbords(node: String): Seq[Edge[String]] = edges.getOrElse(node, Seq.empty)
  }

  object SimpleGraphDB {
    def fromEdges(edges: Edge[String]*): SimpleGraphDB = {
      val edgeMap = edges.groupBy(_.from).view.mapValues(_.toSeq).toMap
      new SimpleGraphDB(edgeMap)
    }
  }

  def runShortestPath(graph: GraphDB[String], start: String, end: String): Option[IndexedSeq[String]] = {
    val stream: ScoredStream[Option[IndexedSeq[String]]] = ShortestPath.shortestPath0(graph, start, end)
    stream.first match {
      case Some(Success(result)) => result
      case _ => None
    }
  }

  test("Float ordering sanity check") {
    val ord = summon[Ordering[Float]]
    // -1 should be greater than -10
    assert(ord.gt(-1.0f, -10.0f), "-1 should be > -10")
    assert(ord.gteq(-1.0f, -10.0f), "-1 should be >= -10")
    assert(!ord.gteq(-10.0f, -1.0f), "-10 should NOT be >= -1")
  }

  test("scoredPure should create element with score") {
    val F = summon[CpsScoredLogicMonad[ScoredStream, Float]]

    // Single element with negative score
    val stream = F.scoredPure("test", -5.0f)
    assertEquals(stream.first, Some(scala.util.Success("test")))
  }

  test("scoredMplus should order by score - higher score first") {
    val F = summon[CpsScoredLogicMonad[ScoredStream, Float]]

    // Create two elements with different scores
    // -1 > -10, so "high" should come first
    val low = F.scoredPure("low", -10.0f)
    val high = F.scoredPure("high", -1.0f)

    val stream = F.scoredMplus(low, -1.0f, high)
    val first = stream.first

    // Debug: what do we actually get?
    println(s"scoredMplus result: $first")
    assertEquals(first, Some(scala.util.Success("high")), "higher score (-1) should come before lower score (-10)")
  }

  test("multiScore should return elements in priority order") {
    val F = summon[CpsScoredLogicMonad[ScoredStream, Float]]

    // Create a stream with two elements: "low" (score -10) and "high" (score -1)
    // Higher score should come first, so "high" should be returned first
    val stream1: ScoredStream[String] = F.multiScore(Seq(
      (-10.0f, () => F.pure("low")),
      (-1.0f, () => F.pure("high"))
    ))
    val first1 = stream1.first
    println(s"multiScore result (low first): $first1")
    assertEquals(first1, Some(scala.util.Success("high")), "low first in seq")

    // Try reverse order
    val stream2: ScoredStream[String] = F.multiScore(Seq(
      (-1.0f, () => F.pure("high")),
      (-10.0f, () => F.pure("low"))
    ))
    val first2 = stream2.first
    println(s"multiScore result (high first): $first2")
    assertEquals(first2, Some(scala.util.Success("high")), "high first in seq")
  }

  test("single edge path - A to B") {
    val graph = SimpleGraphDB.fromEdges(
      Edge("A", "B", 1.0f)
    )

    val result = runShortestPath(graph, "A", "B")

    assertEquals(result, Some(IndexedSeq("A", "B")))
  }

  test("linear path - A to B to C") {
    val graph = SimpleGraphDB.fromEdges(
      Edge("A", "B", 1.0f),
      Edge("B", "C", 1.0f)
    )

    val result = runShortestPath(graph, "A", "C")

    assertEquals(result, Some(IndexedSeq("A", "B", "C")))
  }

  test("diamond graph - should choose shorter path") {
    // A -> B -> D (cost 3 + 3 = 6)
    // A -> C -> D (cost 1 + 1 = 2)
    val graph = SimpleGraphDB.fromEdges(
      Edge("A", "B", 3.0f),
      Edge("A", "C", 1.0f),
      Edge("B", "D", 3.0f),
      Edge("C", "D", 1.0f)
    )

    val result = runShortestPath(graph, "A", "D")

    assertEquals(result, Some(IndexedSeq("A", "C", "D")))
  }

  test("no path exists - disconnected nodes") {
    val graph = SimpleGraphDB.fromEdges(
      Edge("A", "B", 1.0f),
      Edge("C", "D", 1.0f)
    )

    val result = runShortestPath(graph, "A", "D")

    assertEquals(result, None)
  }

  test("start equals end - should return path with just start node") {
    val graph = SimpleGraphDB.fromEdges(
      Edge("A", "B", 1.0f)
    )

    val result = runShortestPath(graph, "A", "A")

    assertEquals(result, Some(IndexedSeq("A")))
  }

  test("multiple paths - verify shortest is chosen") {
    // Multiple paths from A to E:
    // A -> B -> E (cost 1 + 10 = 11)
    // A -> B -> C -> E (cost 1 + 2 + 3 = 6)
    // A -> D -> E (cost 5 + 5 = 10)
    val graph = SimpleGraphDB.fromEdges(
      Edge("A", "B", 1.0f),
      Edge("A", "D", 5.0f),
      Edge("B", "E", 10.0f),
      Edge("B", "C", 2.0f),
      Edge("C", "E", 3.0f),
      Edge("D", "E", 5.0f)
    )

    val result = runShortestPath(graph, "A", "E")

    assertEquals(result, Some(IndexedSeq("A", "B", "C", "E")))
  }

}
