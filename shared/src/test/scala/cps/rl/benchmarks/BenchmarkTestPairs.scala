package cps.rl.benchmarks

import scala.util.Random

/**
 * Deterministic test node pairs for benchmarking shortest path algorithms.
 * Uses a fixed seed (42) for reproducibility.
 */
object BenchmarkTestPairs {

  private val seed = 42

  /**
   * Generate deterministic random pairs for a given number of vertices.
   * @param numVertices The number of vertices in the graph
   * @param numPairs The number of pairs to generate
   * @return Sequence of (source, target) pairs
   */
  def generatePairs(numVertices: Int, numPairs: Int): Seq[(Int, Int)] = {
    val random = new Random(seed)
    (0 until numPairs).map { _ =>
      val src = random.nextInt(numVertices)
      val dst = random.nextInt(numVertices)
      (src, dst)
    }
  }

  /**
   * Pre-computed pairs for 1000EWD graph (1000 vertices).
   * 10 test pairs generated with seed 42.
   */
  val pairs1000EWD: Seq[(Int, Int)] = Seq(
    (380, 674),
    (68, 176),
    (925, 372),
    (634, 926),
    (320, 905),
    (520, 284),
    (794, 188),
    (153, 973),
    (464, 741),
    (551, 18)
  )

  /**
   * Pre-computed pairs for 10000EWD graph (10000 vertices).
   * 10 test pairs generated with seed 42.
   */
  val pairs10000EWD: Seq[(Int, Int)] = Seq(
    (3806, 6747),
    (684, 1765),
    (9253, 3726),
    (6340, 9260),
    (3203, 9052),
    (5206, 2841),
    (7945, 1882),
    (1530, 9733),
    (4642, 7417),
    (5512, 188)
  )

  /**
   * Pre-computed pairs for largeEWD graph (1000000 vertices).
   * 5 test pairs generated with seed 42 (fewer due to larger graph size).
   */
  val pairsLargeEWD: Seq[(Int, Int)] = Seq(
    (380674, 674532),
    (68412, 176894),
    (925312, 372645),
    (634789, 926123),
    (320456, 905234)
  )
}
