package cps.learning.benchmarks

import cps.learning.examples.shortestPath.{Edge, GraphDB}
import scala.io.Source

/**
 * Graph database backed by adjacency list with Int vertices.
 * Designed for loading Princeton algs4 EWD format graphs.
 */
class IntGraphDB(edges: Map[Int, Seq[Edge[Int]]]) extends GraphDB[Int] {
  def neighbords(node: Int): Seq[Edge[Int]] = edges.getOrElse(node, Seq.empty)
  def numVertices: Int = if (edges.isEmpty) 0 else edges.keys.max + 1
  def numEdges: Int = edges.values.map(_.size).sum
}

object IntGraphDB {

  /**
   * Parse Princeton EWD format from string:
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

  /**
   * Load EWD format graph from a classpath resource.
   * @param resourceName The resource name (e.g., "1000EWD.txt")
   * @return The loaded graph
   */
  def fromResource(resourceName: String): IntGraphDB = {
    val stream = getClass.getResourceAsStream(s"/$resourceName")
    if (stream == null) {
      throw new IllegalArgumentException(s"Resource not found: $resourceName")
    }
    try {
      val source = Source.fromInputStream(stream)
      try {
        fromEWDFormat(source.mkString)
      } finally {
        source.close()
      }
    } finally {
      stream.close()
    }
  }

  /**
   * Load EWD format graph from a file path.
   * @param filePath The path to the EWD file
   * @return The loaded graph
   */
  def fromFile(filePath: String): IntGraphDB = {
    val source = Source.fromFile(filePath)
    try {
      fromEWDFormat(source.mkString)
    } finally {
      source.close()
    }
  }
}
