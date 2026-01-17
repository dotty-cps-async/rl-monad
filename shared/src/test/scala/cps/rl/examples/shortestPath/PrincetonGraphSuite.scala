package cps.rl.examples.shortestPath

import munit.*
import scala.util.Success
import cps.*
import cps.monads.{*, given}
import cps.rl.*
import cps.rl.ScoredLogicStreamT.given
import cps.rl.ScalingRing.given
import cps.rl.ds.LogicalSearchPolicy.given

/**
 * Test suite using Princeton algs4 shortest path test data.
 * Data source: https://algs4.cs.princeton.edu/44sp/
 *
 * This suite will be used for benchmarking heap implementations
 * inside CpsSortedLogicMonad.
 */
class PrincetonGraphSuite extends FunSuite {

  type ScoredStream[A] = ScoredLogicStreamT[CpsIdentity, A, Float]

  /**
   * Graph database backed by adjacency list with Int vertices.
   */
  class IntGraphDB(edges: Map[Int, Seq[Edge[Int]]]) extends GraphDB[Int] {
    def neighbords(node: Int): Seq[Edge[Int]] = edges.getOrElse(node, Seq.empty)

    def numVertices: Int = edges.keys.max + 1
    def numEdges: Int = edges.values.map(_.size).sum
  }

  object IntGraphDB {
    /**
     * Parse Princeton EWD format:
     * - First line: number of vertices
     * - Second line: number of edges
     * - Remaining lines: from to weight
     */
    def fromEWDFormat(data: String): IntGraphDB = {
      val lines = data.trim.split("\n").map(_.trim).filter(_.nonEmpty)
      val numVertices = lines(0).toInt
      val numEdges = lines(1).toInt

      val edges = lines.drop(2).map { line =>
        val parts = line.split("\\s+")
        Edge(parts(0).toInt, parts(1).toInt, parts(2).toFloat)
      }

      val edgeMap = edges.groupBy(_.from).view.mapValues(_.toSeq).toMap
      new IntGraphDB(edgeMap)
    }

    def fromEdges(edges: Edge[Int]*): IntGraphDB = {
      val edgeMap = edges.groupBy(_.from).view.mapValues(_.toSeq).toMap
      new IntGraphDB(edgeMap)
    }
  }

  /**
   * tinyEWD.txt - 8 vertices, 15 edges
   * Source: https://algs4.cs.princeton.edu/44sp/tinyEWD.txt
   */
  val tinyEWD: String = """
    |8
    |15
    |4 5 0.35
    |5 4 0.35
    |4 7 0.37
    |5 7 0.28
    |7 5 0.28
    |5 1 0.32
    |0 4 0.38
    |0 2 0.26
    |7 3 0.39
    |1 3 0.29
    |2 7 0.34
    |6 2 0.40
    |3 6 0.52
    |6 0 0.58
    |6 4 0.93
    """.stripMargin

  /**
   * Expected shortest paths from vertex 0 in tinyEWD.
   * Computed using Dijkstra's algorithm:
   *
   * Vertex | Distance | Path
   * -------|----------|------
   *   0    |   0.00   | 0
   *   1    |   1.05   | 0->4->5->1
   *   2    |   0.26   | 0->2
   *   3    |   0.99   | 0->2->7->3
   *   4    |   0.38   | 0->4
   *   5    |   0.73   | 0->4->5
   *   6    |   1.51   | 0->2->7->3->6
   *   7    |   0.60   | 0->2->7
   */
  val expectedDistancesFromZero: Map[Int, Float] = Map(
    0 -> 0.00f,
    1 -> 1.05f,
    2 -> 0.26f,
    3 -> 0.99f,
    4 -> 0.38f,
    5 -> 0.73f,
    6 -> 1.51f,
    7 -> 0.60f
  )

  val expectedPathsFromZero: Map[Int, IndexedSeq[Int]] = Map(
    0 -> IndexedSeq(0),
    1 -> IndexedSeq(0, 4, 5, 1),
    2 -> IndexedSeq(0, 2),
    3 -> IndexedSeq(0, 2, 7, 3),
    4 -> IndexedSeq(0, 4),
    5 -> IndexedSeq(0, 4, 5),
    6 -> IndexedSeq(0, 2, 7, 3, 6),
    7 -> IndexedSeq(0, 2, 7)
  )

  def runShortestPath(graph: GraphDB[Int], start: Int, end: Int): Option[(IndexedSeq[Int], Float)] = {
    val stream: ScoredStream[Option[IndexedSeq[Int]]] = ShortestPath.shortestPath(graph, start, end)
    stream.first match {
      case Some(Success(Some(path))) =>
        // Compute distance from path
        val dist = path.sliding(2).collect {
          case IndexedSeq(from, to) =>
            graph.neighbords(from).find(_.to == to).map(_.cost).getOrElse(0.0f)
        }.sum
        Some((path, dist))
      case _ => None
    }
  }

  lazy val tinyGraph: IntGraphDB = IntGraphDB.fromEWDFormat(tinyEWD)

  test("tinyEWD: parse graph correctly") {
    assertEquals(tinyGraph.numVertices, 8)
    assertEquals(tinyGraph.numEdges, 15)

    // Verify some edges
    val edges0 = tinyGraph.neighbords(0)
    assertEquals(edges0.size, 2)
    assert(edges0.exists(e => e.to == 4 && math.abs(e.cost - 0.38f) < 0.001f))
    assert(edges0.exists(e => e.to == 2 && math.abs(e.cost - 0.26f) < 0.001f))
  }

  test("tinyEWD: shortest path 0 -> 0") {
    val result = runShortestPath(tinyGraph, 0, 0)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, IndexedSeq(0))
    assert(math.abs(dist - 0.0f) < 0.001f, s"Expected 0.0, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 2 (direct edge)") {
    val result = runShortestPath(tinyGraph, 0, 2)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(2))
    assert(math.abs(dist - expectedDistancesFromZero(2)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(2)}, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 4 (direct edge)") {
    val result = runShortestPath(tinyGraph, 0, 4)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(4))
    assert(math.abs(dist - expectedDistancesFromZero(4)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(4)}, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 7 (via 2)") {
    val result = runShortestPath(tinyGraph, 0, 7)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(7))
    assert(math.abs(dist - expectedDistancesFromZero(7)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(7)}, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 5 (via 4)") {
    val result = runShortestPath(tinyGraph, 0, 5)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(5))
    assert(math.abs(dist - expectedDistancesFromZero(5)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(5)}, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 1 (via 4, 5)") {
    val result = runShortestPath(tinyGraph, 0, 1)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(1))
    assert(math.abs(dist - expectedDistancesFromZero(1)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(1)}, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 3 (via 2, 7)") {
    val result = runShortestPath(tinyGraph, 0, 3)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(3))
    assert(math.abs(dist - expectedDistancesFromZero(3)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(3)}, got $dist")
  }

  test("tinyEWD: shortest path 0 -> 6 (via 2, 7, 3)") {
    val result = runShortestPath(tinyGraph, 0, 6)
    assert(result.isDefined, "Path should exist")
    val (path, dist) = result.get
    assertEquals(path, expectedPathsFromZero(6))
    assert(math.abs(dist - expectedDistancesFromZero(6)) < 0.001f,
      s"Expected ${expectedDistancesFromZero(6)}, got $dist")
  }

  test("tinyEWD: all shortest paths from vertex 0") {
    for (target <- 0 until 8) {
      val result = runShortestPath(tinyGraph, 0, target)
      assert(result.isDefined, s"Path to $target should exist")
      val (path, dist) = result.get
      assertEquals(path, expectedPathsFromZero(target), s"Wrong path to $target")
      assert(math.abs(dist - expectedDistancesFromZero(target)) < 0.001f,
        s"Wrong distance to $target: expected ${expectedDistancesFromZero(target)}, got $dist")
    }
  }

}
