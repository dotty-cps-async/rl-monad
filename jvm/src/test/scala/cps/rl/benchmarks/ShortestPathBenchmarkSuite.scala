package cps.rl.benchmarks

import munit.*
import scala.util.{Success, Failure, Try}
import scala.util.control.TailCalls.*
import cps.*
import cps.monads.{*, given}
import cps.rl.*
import cps.rl.ds.LogicalSearchPolicy.given
import cps.rl.examples.shortestPath.{Edge, GraphDB, ShortestPath}

/**
 * Benchmark suite for comparing priority queue implementations in the scored logic monad.
 *
 * Tests shortest path algorithm across different graph sizes:
 * - 1000EWD.txt: 1,000 vertices, 16,866 edges
 * - 10000EWD.txt: 10,000 vertices, 123,462 edges
 * - largeEWD.txt: 1,000,000 vertices, 15,172,126 edges
 *
 * Uses deterministic node pairs from BenchmarkTestPairs for reproducibility.
 */
class ShortestPathBenchmarkSuite extends FunSuite {

  // Increase timeout for large graph tests
  override val munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(10, "min")

  // Lazy loading of graphs to avoid memory issues
  lazy val graph1000: IntGraphDB = IntGraphDB.fromResource("1000EWD.txt")
  lazy val graph10000: IntGraphDB = IntGraphDB.fromResource("10000EWD.txt")

  // Check if largeEWD exists without loading it (to avoid OOM on check)
  lazy val largeEWDExists: Boolean = {
    val resourcePath = "largeEWD.txt"
    Option(getClass.getClassLoader.getResourceAsStream(resourcePath)).map(_.close()).isDefined
  }

  // Check if we have enough heap memory for largeEWD (needs ~8GB)
  lazy val hasEnoughMemoryForLargeEWD: Boolean = {
    val maxHeapMB = Runtime.getRuntime.maxMemory() / 1024 / 1024
    maxHeapMB >= 7000  // Require at least 7GB
  }

  // largeEWD might not be available in all environments (removed from git due to size)
  // Only load if it exists - loading requires significant heap memory (-Xmx8g recommended)
  lazy val graphLarge: IntGraphDB = {
    require(largeEWDExists, "largeEWD.txt not found")
    IntGraphDB.fromResource("largeEWD.txt")
  }

  case class BenchmarkResult(
    implementation: String,
    graphName: String,
    pair: (Int, Int),
    pathFound: Boolean,
    pathLength: Option[Int],
    timeMs: Long,
    error: Option[String] = None
  )

  def formatResults(results: Seq[BenchmarkResult]): String = {
    val header = f"| ${"Implementation"}%-25s | ${"Graph"}%-12s | ${"Source"}%8s | ${"Target"}%8s | ${"Found"}%-5s | ${"PathLen"}%7s | ${"Time (ms)"}%10s | ${"Error"}%-15s |"
    val separator = "|" + "-" * 27 + "|" + "-" * 14 + "|" + "-" * 10 + "|" + "-" * 10 + "|" + "-" * 7 + "|" + "-" * 9 + "|" + "-" * 12 + "|" + "-" * 17 + "|"

    val rows = results.map { r =>
      f"| ${r.implementation}%-25s | ${r.graphName}%-12s | ${r.pair._1}%8d | ${r.pair._2}%8d | ${r.pathFound}%-5s | ${r.pathLength.map(_.toString).getOrElse("-")}%7s | ${r.timeMs}%10d | ${r.error.getOrElse("-")}%-15s |"
    }

    (header +: separator +: rows).mkString("\n")
  }

  // ============== PairingHeapStreamModule Benchmarks ==============

  def runPairingHeapBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    import PairingHeapStreamModule.given

    type PairingStream[A] = PairingHeapStreamModule.Stream[CpsIdentity, A, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val stream: PairingStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[PairingStream, Int](graph, src, dst)

        val result = stream.first
        val endTime = System.currentTimeMillis()

        result match {
          case Some(Success(Some(path))) =>
            BenchmarkResult("PairingHeapStreamModule", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case Some(Success(None)) =>
            BenchmarkResult("PairingHeapStreamModule", graphName, (src, dst), false, None, endTime - startTime)
          case Some(Failure(e)) =>
            BenchmarkResult("PairingHeapStreamModule", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
          case None =>
            BenchmarkResult("PairingHeapStreamModule", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("PairingHeapStreamModule", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("PairingHeapStreamModule", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== PairingHeapStreamModule Iterative Benchmarks ==============

  def runPairingHeapIterativeBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    import PairingHeapStreamModule.given

    type PairingStream[A] = PairingHeapStreamModule.Stream[CpsIdentity, A, Float]

    // Create the monad instance with the refined Observer type
    given pairingMonad: (CpsScoredLogicMonad[PairingStream, Float] { type Observer[X] = CpsIdentity[X] }) =
      PairingHeapStreamModule.cpsScoredLogicMonad[CpsIdentity, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val result: CpsIdentity[Option[IndexedSeq[Int]]] =
          ShortestPathIterative.shortestPath[PairingStream, CpsIdentity, Int](graph, src, dst)

        val endTime = System.currentTimeMillis()

        result match {
          case Some(path) =>
            BenchmarkResult("PairingHeap-Iterative", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case None =>
            BenchmarkResult("PairingHeap-Iterative", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("PairingHeap-Iterative", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("PairingHeap-Iterative", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== FingerTreeStreamModule Iterative Benchmarks ==============

  def runFingerTreeIterativeBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    import FingerTreeStreamModule.given

    type FingerTreeStream[A] = FingerTreeStreamModule.Stream[CpsIdentity, A, Float]

    // Create the monad instance with the refined Observer type
    given fingerTreeMonad: (CpsScoredLogicMonad[FingerTreeStream, Float] { type Observer[X] = CpsIdentity[X] }) =
      FingerTreeStreamModule.cpsScoredLogicMonad[CpsIdentity, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val result: CpsIdentity[Option[IndexedSeq[Int]]] =
          ShortestPathIterative.shortestPath[FingerTreeStream, CpsIdentity, Int](graph, src, dst)

        val endTime = System.currentTimeMillis()

        result match {
          case Some(path) =>
            BenchmarkResult("FingerTree-Iterative", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case None =>
            BenchmarkResult("FingerTree-Iterative", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("FingerTree-Iterative", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("FingerTree-Iterative", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== PairingHeapStreamModule with TailRec (stack-safe) ==============

  def runPairingHeapTailRecBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    // Use TailRec as the outer monad for stack-safe evaluation
    type TailRecStream[A] = PairingHeapStreamModule.Stream[TailRec, A, Float]

    given PairingHeapStreamModule.StreamMonad[TailRec, Float] =
      PairingHeapStreamModule.cpsScoredLogicMonad[TailRec, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val stream: TailRecStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[TailRecStream, Int](graph, src, dst)

        // Evaluate the TailRec to get the result
        val tailRecResult: TailRec[Option[Try[Option[IndexedSeq[Int]]]]] = stream.first
        val result = tailRecResult.result  // This trampolines the evaluation
        val endTime = System.currentTimeMillis()

        result match {
          case Some(Success(Some(path))) =>
            BenchmarkResult("PairingHeap-TailRec", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case Some(Success(None)) =>
            BenchmarkResult("PairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime)
          case Some(Failure(e)) =>
            BenchmarkResult("PairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
          case None =>
            BenchmarkResult("PairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("PairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("PairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== FingerTreeStreamModule with TailRec (stack-safe) ==============

  def runFingerTreeTailRecBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    // Use TailRec as the outer monad for stack-safe evaluation
    type TailRecStream[A] = FingerTreeStreamModule.Stream[TailRec, A, Float]

    given FingerTreeStreamModule.StreamMonad[TailRec, Float] =
      FingerTreeStreamModule.cpsScoredLogicMonad[TailRec, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val stream: TailRecStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[TailRecStream, Int](graph, src, dst)

        // Evaluate the TailRec to get the result
        val tailRecResult: TailRec[Option[Try[Option[IndexedSeq[Int]]]]] = stream.first
        val result = tailRecResult.result  // This trampolines the evaluation
        val endTime = System.currentTimeMillis()

        result match {
          case Some(Success(Some(path))) =>
            BenchmarkResult("FingerTree-TailRec", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case Some(Success(None)) =>
            BenchmarkResult("FingerTree-TailRec", graphName, (src, dst), false, None, endTime - startTime)
          case Some(Failure(e)) =>
            BenchmarkResult("FingerTree-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
          case None =>
            BenchmarkResult("FingerTree-TailRec", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("FingerTree-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("FingerTree-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== FingerTreeStreamModule Benchmarks ==============

  def runFingerTreeBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    import FingerTreeStreamModule.given

    type FingerTreeStream[A] = FingerTreeStreamModule.Stream[CpsIdentity, A, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val stream: FingerTreeStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[FingerTreeStream, Int](graph, src, dst)

        val result = stream.first
        val endTime = System.currentTimeMillis()

        result match {
          case Some(Success(Some(path))) =>
            BenchmarkResult("FingerTreeStreamModule", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case Some(Success(None)) =>
            BenchmarkResult("FingerTreeStreamModule", graphName, (src, dst), false, None, endTime - startTime)
          case Some(Failure(e)) =>
            BenchmarkResult("FingerTreeStreamModule", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
          case None =>
            BenchmarkResult("FingerTreeStreamModule", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("FingerTreeStreamModule", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("FingerTreeStreamModule", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== SimplePairingHeapStreamModule Benchmarks ==============

  def runSimplePairingHeapBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    import SimplePairingHeapStreamModule.given

    type SimplePairingStream[A] = SimplePairingHeapStreamModule.Stream[CpsIdentity, A, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val stream: SimplePairingStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[SimplePairingStream, Int](graph, src, dst)

        val result = stream.first
        val endTime = System.currentTimeMillis()

        result match {
          case Some(Success(Some(path))) =>
            BenchmarkResult("SimplePairingHeap", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case Some(Success(None)) =>
            BenchmarkResult("SimplePairingHeap", graphName, (src, dst), false, None, endTime - startTime)
          case Some(Failure(e)) =>
            BenchmarkResult("SimplePairingHeap", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
          case None =>
            BenchmarkResult("SimplePairingHeap", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("SimplePairingHeap", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("SimplePairingHeap", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== SimplePairingHeapStreamModule with TailRec (stack-safe) ==============

  def runSimplePairingHeapTailRecBenchmark(graph: IntGraphDB, pairs: Seq[(Int, Int)], graphName: String): Seq[BenchmarkResult] = {
    type TailRecStream[A] = SimplePairingHeapStreamModule.Stream[TailRec, A, Float]

    given SimplePairingHeapStreamModule.StreamMonad[TailRec, Float] =
      SimplePairingHeapStreamModule.cpsScoredLogicMonad[TailRec, Float]

    pairs.map { case (src, dst) =>
      val startTime = System.currentTimeMillis()

      try {
        val stream: TailRecStream[Option[IndexedSeq[Int]]] =
          ShortestPath.shortestPath[TailRecStream, Int](graph, src, dst)

        val tailRecResult: TailRec[Option[Try[Option[IndexedSeq[Int]]]]] = stream.first
        val result = tailRecResult.result
        val endTime = System.currentTimeMillis()

        result match {
          case Some(Success(Some(path))) =>
            BenchmarkResult("SimplePairingHeap-TailRec", graphName, (src, dst), true, Some(path.length), endTime - startTime)
          case Some(Success(None)) =>
            BenchmarkResult("SimplePairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime)
          case Some(Failure(e)) =>
            BenchmarkResult("SimplePairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
          case None =>
            BenchmarkResult("SimplePairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime)
        }
      } catch {
        case e: StackOverflowError =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("SimplePairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some("StackOverflow"))
        case e: Throwable =>
          val endTime = System.currentTimeMillis()
          BenchmarkResult("SimplePairingHeap-TailRec", graphName, (src, dst), false, None, endTime - startTime, Some(e.getClass.getSimpleName))
      }
    }
  }

  // ============== 1000EWD Benchmarks ==============

  test("1000EWD: PairingHeapStreamModule benchmark") {
    val results = runPairingHeapBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("1000EWD: FingerTreeStreamModule benchmark") {
    val results = runFingerTreeBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("1000EWD: SimplePairingHeap benchmark") {
    val results = runSimplePairingHeapBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("1000EWD: Compare implementations") {
    val pairingResults = runPairingHeapBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    val simplePairingResults = runSimplePairingHeapBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    val fingerTreeResults = runFingerTreeBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")

    println("\n=== 1000EWD Comparison ===")
    println(formatResults(pairingResults ++ simplePairingResults ++ fingerTreeResults))

    val pairingTotal = pairingResults.map(_.timeMs).sum
    val simplePairingTotal = simplePairingResults.map(_.timeMs).sum
    val fingerTreeTotal = fingerTreeResults.map(_.timeMs).sum
    println(f"\nBootstrappedPairingHeap total: ${pairingTotal}ms")
    println(f"SimplePairingHeap total: ${simplePairingTotal}ms")
    println(f"FingerTree total: ${fingerTreeTotal}ms")
    if (pairingTotal > 0) {
      println(f"SimplePairingHeap vs Bootstrapped: ${pairingTotal.toDouble / simplePairingTotal.toDouble}%.2fx")
      println(f"FingerTree vs Bootstrapped: ${fingerTreeTotal.toDouble / pairingTotal.toDouble}%.2fx")
    }
  }

  // ============== 10000EWD Benchmarks ==============

  test("10000EWD: PairingHeapStreamModule benchmark") {
    val results = runPairingHeapBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("10000EWD: FingerTreeStreamModule benchmark") {
    val results = runFingerTreeBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("10000EWD: SimplePairingHeap benchmark") {
    val results = runSimplePairingHeapBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("10000EWD: Compare implementations") {
    val pairingResults = runPairingHeapBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    val simplePairingResults = runSimplePairingHeapBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    val fingerTreeResults = runFingerTreeBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")

    println("\n=== 10000EWD Comparison ===")
    println(formatResults(pairingResults ++ simplePairingResults ++ fingerTreeResults))

    val pairingTotal = pairingResults.map(_.timeMs).sum
    val simplePairingTotal = simplePairingResults.map(_.timeMs).sum
    val fingerTreeTotal = fingerTreeResults.map(_.timeMs).sum
    println(f"\nBootstrappedPairingHeap total: ${pairingTotal}ms")
    println(f"SimplePairingHeap total: ${simplePairingTotal}ms")
    println(f"FingerTree total: ${fingerTreeTotal}ms")
    if (pairingTotal > 0) {
      println(f"SimplePairingHeap vs Bootstrapped: ${pairingTotal.toDouble / simplePairingTotal.toDouble}%.2fx")
      println(f"FingerTree vs Bootstrapped: ${fingerTreeTotal.toDouble / pairingTotal.toDouble}%.2fx")
    }
  }

  // ============== largeEWD Benchmarks (optional, may not be available) ==============

  test("largeEWD: PairingHeapStreamModule benchmark".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    val results = runPairingHeapBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
  }

  test("largeEWD: FingerTreeStreamModule benchmark".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    val results = runFingerTreeBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
  }

  test("largeEWD: SimplePairingHeap benchmark".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    val results = runSimplePairingHeapBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
  }

  test("largeEWD: PairingHeap-TailRec benchmark (stack-safe)".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    println("\n=== TailRec (trampolined) evaluation for stack safety ===")
    val results = runPairingHeapTailRecBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("largeEWD: FingerTree-TailRec benchmark (stack-safe)".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    println("\n=== FingerTree with TailRec (trampolined) ===")
    val results = runFingerTreeTailRecBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("largeEWD: SimplePairingHeap-TailRec benchmark (stack-safe)".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    println("\n=== SimplePairingHeap with TailRec (trampolined) ===")
    val results = runSimplePairingHeapTailRecBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    println("\n" + formatResults(results))

    val totalTime = results.map(_.timeMs).sum
    val avgTime = totalTime.toDouble / results.length
    val foundCount = results.count(_.pathFound)
    val errorCount = results.count(_.error.isDefined)
    println(f"\nTotal time: ${totalTime}ms, Average: ${avgTime}%.2fms")
    println(f"Paths found: $foundCount/${results.length}, Errors: $errorCount")
  }

  test("largeEWD: Compare TailRec implementations".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    println("\n=== largeEWD TailRec Comparison ===")
    val pairingResults = runPairingHeapTailRecBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    val simplePairingResults = runSimplePairingHeapTailRecBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    val fingerTreeResults = runFingerTreeTailRecBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")

    println(formatResults(pairingResults ++ simplePairingResults ++ fingerTreeResults))

    val pairingTotal = pairingResults.map(_.timeMs).sum
    val simplePairingTotal = simplePairingResults.map(_.timeMs).sum
    val fingerTreeTotal = fingerTreeResults.map(_.timeMs).sum
    println(f"\nBootstrappedPairingHeap-TailRec total: ${pairingTotal}ms")
    println(f"SimplePairingHeap-TailRec total: ${simplePairingTotal}ms")
    println(f"FingerTree-TailRec total: ${fingerTreeTotal}ms")
    if (pairingTotal > 0) {
      println(f"SimplePairingHeap vs Bootstrapped: ${pairingTotal.toDouble / simplePairingTotal.toDouble}%.2fx")
      println(f"FingerTree vs Bootstrapped: ${fingerTreeTotal.toDouble / pairingTotal.toDouble}%.2fx")
    }
  }

  test("largeEWD: Compare implementations".tag(munit.Slow)) {
    assume(largeEWDExists, "largeEWD.txt not found. Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to run this test.")
    assume(hasEnoughMemoryForLargeEWD, s"Not enough heap memory. Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    val pairingResults = runPairingHeapBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    val simplePairingResults = runSimplePairingHeapBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
    val fingerTreeResults = runFingerTreeBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")

    println("\n=== largeEWD Comparison ===")
    println(formatResults(pairingResults ++ simplePairingResults ++ fingerTreeResults))

    val pairingTotal = pairingResults.map(_.timeMs).sum
    val simplePairingTotal = simplePairingResults.map(_.timeMs).sum
    val fingerTreeTotal = fingerTreeResults.map(_.timeMs).sum
    println(f"\nBootstrappedPairingHeap total: ${pairingTotal}ms")
    println(f"SimplePairingHeap total: ${simplePairingTotal}ms")
    println(f"FingerTree total: ${fingerTreeTotal}ms")
    if (pairingTotal > 0) {
      println(f"SimplePairingHeap vs Bootstrapped: ${pairingTotal.toDouble / simplePairingTotal.toDouble}%.2fx")
      println(f"FingerTree vs Bootstrapped: ${fingerTreeTotal.toDouble / pairingTotal.toDouble}%.2fx")
    }
  }

  // ============== Summary Test ==============

  test("Full benchmark summary") {
    println("\n" + "=" * 80)
    println("SHORTEST PATH BENCHMARK SUMMARY")
    println("=" * 80)

    var allResults = Seq.empty[BenchmarkResult]

    // 1000EWD
    println("\n--- 1000EWD (1,000 vertices, 16,866 edges) ---")
    val results1000Pairing = runPairingHeapBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    val results1000SimplePairing = runSimplePairingHeapBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    val results1000Finger = runFingerTreeBenchmark(graph1000, BenchmarkTestPairs.pairs1000EWD, "1000EWD")
    allResults = allResults ++ results1000Pairing ++ results1000SimplePairing ++ results1000Finger

    // 10000EWD
    println("--- 10000EWD (10,000 vertices, 123,462 edges) ---")
    val results10000Pairing = runPairingHeapBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    val results10000SimplePairing = runSimplePairingHeapBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    val results10000Finger = runFingerTreeBenchmark(graph10000, BenchmarkTestPairs.pairs10000EWD, "10000EWD")
    allResults = allResults ++ results10000Pairing ++ results10000SimplePairing ++ results10000Finger

    // largeEWD (if available and enough memory)
    if (largeEWDExists && hasEnoughMemoryForLargeEWD) {
      println("--- largeEWD (1,000,000 vertices, 15,172,126 edges) ---")
      val resultsLargePairing = runPairingHeapBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
      val resultsLargeSimplePairing = runSimplePairingHeapBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
      val resultsLargeFinger = runFingerTreeBenchmark(graphLarge, BenchmarkTestPairs.pairsLargeEWD, "largeEWD")
      allResults = allResults ++ resultsLargePairing ++ resultsLargeSimplePairing ++ resultsLargeFinger
    } else if (!largeEWDExists) {
      println("--- largeEWD: SKIPPED (file not found) ---")
      println("    Download from https://algs4.cs.princeton.edu/44sp/largeEWD.txt to include in benchmarks")
    } else {
      println("--- largeEWD: SKIPPED (not enough heap memory) ---")
      println(s"    Need at least 7GB, have ${Runtime.getRuntime.maxMemory() / 1024 / 1024}MB. Run with -Xmx8g.")
    }

    println("\n" + formatResults(allResults))

    // Summary statistics
    println("\n" + "=" * 80)
    println("AGGREGATE STATISTICS")
    println("=" * 80)

    val byImpl = allResults.groupBy(_.implementation)
    for ((impl, results) <- byImpl) {
      val totalTime = results.map(_.timeMs).sum
      val avgTime = totalTime.toDouble / results.length
      val pathsFound = results.count(_.pathFound)
      println(f"\n$impl:")
      println(f"  Total time: ${totalTime}ms")
      println(f"  Average time: ${avgTime}%.2fms")
      println(f"  Paths found: $pathsFound/${results.length}")
    }

    val byGraph = allResults.groupBy(_.graphName)
    for ((graph, results) <- byGraph.toSeq.sortBy(_._1)) {
      println(f"\n$graph:")
      val byImplInGraph = results.groupBy(_.implementation)
      for ((impl, implResults) <- byImplInGraph) {
        val totalTime = implResults.map(_.timeMs).sum
        println(f"  $impl: ${totalTime}ms total")
      }
    }
  }
}
