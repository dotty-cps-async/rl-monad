package cps.rl.examples.tiktaktoe

import java.lang.foreign.*
import java.lang.foreign.ValueLayout.*
import java.nio.file.Path

object DirectPyTorchTest {

  def main(args: Array[String]): Unit = {
    println("=== Direct PyTorch Load Test (Panama FFM) ===")

    val libPath = sys.env.getOrElse("PYTORCH_LIBRARY_PATH", "lib/libtorch/lib")
    println(s"Library path: $libPath")

    try {
      // Load core libraries in order
      val libs = Seq(
        "libc10.so",
        "libtorch_cpu.so",
        "libtorch.so"
      )

      libs.foreach { lib =>
        val fullPath = s"$libPath/$lib"
        println(s"Loading $fullPath ...")
        try {
          System.load(fullPath)
          println(s"  OK: $lib loaded")
        } catch {
          case e: UnsatisfiedLinkError =>
            println(s"  WARN: $lib - ${e.getMessage}")
        }
      }

      // Try to load CUDA runtime
      val cudartPath = s"$libPath/libcudart*.so*"
      val cudartFiles = new java.io.File(libPath).listFiles()
        .filter(_.getName.startsWith("libcudart"))
        .filter(f => f.getName.endsWith(".so") || f.getName.contains(".so."))
        .sortBy(_.getName.length)

      cudartFiles.headOption.foreach { cudart =>
        println(s"Loading ${cudart.getAbsolutePath} ...")
        try {
          System.load(cudart.getAbsolutePath)
          println(s"  OK: cudart loaded")
        } catch {
          case e: UnsatisfiedLinkError =>
            println(s"  WARN: cudart - ${e.getMessage}")
        }
      }

      // Try to load CUDA version
      val cudaLibs = Seq(
        "libc10_cuda.so",
        "libtorch_cuda.so"
      )

      cudaLibs.foreach { lib =>
        val fullPath = s"$libPath/$lib"
        println(s"Loading $fullPath ...")
        try {
          System.load(fullPath)
          println(s"  OK: $lib loaded")
        } catch {
          case e: UnsatisfiedLinkError =>
            println(s"  WARN: $lib - ${e.getMessage}")
        }
      }

      println("\n=== Libraries loaded successfully ===")

      // Now call cudaGetDeviceCount via Panama FFM
      println("\n=== Calling CUDA functions ===")

      val linker = Linker.nativeLinker()
      val arena = Arena.ofConfined()

      // Find cudaGetDeviceCount symbol
      val loaderLookup = SymbolLookup.loaderLookup()

      val cudaGetDeviceCount = loaderLookup.find("cudaGetDeviceCount")

      if (cudaGetDeviceCount.isPresent) {
        println("Found cudaGetDeviceCount")

        // cudaError_t cudaGetDeviceCount(int* count)
        val descriptor = FunctionDescriptor.of(JAVA_INT, ADDRESS)
        val handle = linker.downcallHandle(cudaGetDeviceCount.get(), descriptor)

        // Allocate int for result
        val countPtr = arena.allocate(JAVA_INT)

        val result = handle.invoke(countPtr).asInstanceOf[Int]
        val count = countPtr.get(JAVA_INT, 0)

        println(s"cudaGetDeviceCount returned: $result (0=success)")
        println(s"GPU count: $count")
      } else {
        println("cudaGetDeviceCount not found")

        // List available symbols containing "cuda"
        println("\nSearching for cuda symbols...")
      }

      println("\n=== Test Complete ===")

    } catch {
      case e: Exception =>
        println(s"\n=== Test FAILED ===")
        println(s"Error: ${e.getMessage}")
        e.printStackTrace()
    }
  }
}
