package cps.learning.examples.shortestPath

import munit.*
import scala.util.Success
import cps.*
import cps.monads.{*, given}
import cps.learning.*
import cps.learning.ds.LogicalSearchPolicy.given

/**
 * Test suite demonstrating the module pattern for benchmarking different
 * heap implementations in the scored logic monad.
 *
 * This suite runs the same shortest path algorithm using:
 * 1. PairingHeapStreamModule - the default implementation using bootstrapped pairing heaps
 * 2. FingerTreeStreamModule - alternative implementation using finger trees
 */
class ModuleBenchmarkSuite extends FunSuite {

  /**
   * tinyEWD.txt - 8 vertices, 15 edges
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

  class IntGraphDB(edges: Map[Int, Seq[Edge[Int]]]) extends GraphDB[Int] {
    def neighbords(node: Int): Seq[Edge[Int]] = edges.getOrElse(node, Seq.empty)
    def numVertices: Int = edges.keys.max + 1
    def numEdges: Int = edges.values.map(_.size).sum
  }

  object IntGraphDB {
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
  }

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

  lazy val tinyGraph: IntGraphDB = IntGraphDB.fromEWDFormat(tinyEWD)

  // ============== PairingHeap Module Tests ==============

  test("PairingHeapStreamModule: shortest path 0 -> 6") {
    import PairingHeapStreamModule.given

    type PairingStream[A] = PairingHeapStreamModule.Stream[CpsIdentity, A, Float]

    val stream: PairingStream[Option[IndexedSeq[Int]]] =
      ShortestPath.shortestPath[PairingStream, Int](tinyGraph, 0, 6)

    stream.first match {
      case Some(Success(Some(path))) =>
        assertEquals(path, expectedPathsFromZero(6))
      case other =>
        fail(s"Expected path, got $other")
    }
  }

  test("PairingHeapStreamModule: all shortest paths from vertex 0") {
    import PairingHeapStreamModule.given

    type PairingStream[A] = PairingHeapStreamModule.Stream[CpsIdentity, A, Float]

    for (target <- 0 until 8) {
      val stream: PairingStream[Option[IndexedSeq[Int]]] =
        ShortestPath.shortestPath[PairingStream, Int](tinyGraph, 0, target)

      stream.first match {
        case Some(Success(Some(path))) =>
          assertEquals(path, expectedPathsFromZero(target), s"Wrong path to $target")
        case other =>
          fail(s"Expected path to $target, got $other")
      }
    }
  }

  // ============== FingerTree Module Tests ==============

  test("FingerTreeStreamModule: shortest path 0 -> 6") {
    import FingerTreeStreamModule.given

    type FingerTreeStream[A] = FingerTreeStreamModule.Stream[CpsIdentity, A, Float]

    val stream: FingerTreeStream[Option[IndexedSeq[Int]]] =
      ShortestPath.shortestPath[FingerTreeStream, Int](tinyGraph, 0, 6)

    stream.first match {
      case Some(Success(Some(path))) =>
        assertEquals(path, expectedPathsFromZero(6))
      case other =>
        fail(s"Expected path, got $other")
    }
  }

  test("FingerTreeStreamModule: all shortest paths from vertex 0") {
    import FingerTreeStreamModule.given

    type FingerTreeStream[A] = FingerTreeStreamModule.Stream[CpsIdentity, A, Float]

    for (target <- 0 until 8) {
      val stream: FingerTreeStream[Option[IndexedSeq[Int]]] =
        ShortestPath.shortestPath[FingerTreeStream, Int](tinyGraph, 0, target)

      stream.first match {
        case Some(Success(Some(path))) =>
          assertEquals(path, expectedPathsFromZero(target), s"Wrong path to $target")
        case other =>
          fail(s"Expected path to $target, got $other")
      }
    }
  }

  // ============== Comparative Benchmark (same results, different implementations) ==============

  test("Both modules produce identical results") {
    for (target <- 0 until 8) {
      // Run with PairingHeap
      val pairingPath = {
        import PairingHeapStreamModule.given
        type PairingStream[A] = PairingHeapStreamModule.Stream[CpsIdentity, A, Float]
        val stream: PairingStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[PairingStream, Int](tinyGraph, 0, target)
        stream.first
      }

      // Run with FingerTree
      val fingerTreePath = {
        import FingerTreeStreamModule.given
        type FingerTreeStream[A] = FingerTreeStreamModule.Stream[CpsIdentity, A, Float]
        val stream: FingerTreeStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[FingerTreeStream, Int](tinyGraph, 0, target)
        stream.first
      }

      // Results should be identical
      assertEquals(pairingPath, fingerTreePath, s"Different results for path to $target")
    }
  }

}
