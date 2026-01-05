# Tensor Memory Management Design

## Problem Statement

Current `TensorRepresentation` design has memory management issues:

1. **Captured Manager**: The trait captures `NDManager` at instantiation time via `given (using NDManager)`, but tensors need lifecycle management by a different manager (trainer's subManager).

2. **Error-Prone Cleanup**: Callers must manually attach tensors to subManager or close them explicitly - easy to forget, leading to memory leaks.

```scala
// Current problematic pattern
given (using NDManager): TensorRepresentation[Board] with {
  def toTensor(a: Board): NDArray = summon[NDManager].create(arr)  // Uses captured manager
}

// Caller must remember to fix lifecycle
val tensor = obsRepr.toTensor(observation)
tensor.attach(subManager)  // Easy to forget!
```

## Proposed Design

### Design Decision: Type Classes

Type classes allow platform-specific implementations without inheritance:
- `TensorScope[S]` - type class for scope lifecycle operations (create sub-scope, close)
- `TensorRepresentation[A, S: TensorScope]` - type class for tensor conversion, context bound ensures `S` has `TensorScope`
- `TensorPlatform` - trait for platform abstraction (provides root scope type and factory)
- Instances found via implicit search, extension methods for clean syntax

### Core Abstractions

```scala
// shared/src/main/scala/cps/learning/TensorScope.scala

/**
 * Type class for tensor memory scope management.
 */
trait TensorScope[S] {
  def subScope(s: S): S
  def close(s: S): Unit

  def withScope[A](parent: S)(f: S => A): A = {
    val sub = subScope(parent)
    try f(sub)
    finally close(sub)
  }
}

/**
 * Type class for converting domain objects to/from tensors.
 * Context bound [S: TensorScope] ensures S has a TensorScope instance.
 */
trait TensorRepresentation[A, S: TensorScope] {
  type Tensor

  def toTensor(a: A, scope: S): Tensor
  def fromTensor(t: Tensor): Option[A]
  def tensorShape(a: A): Seq[Int]
}

/**
 * Platform abstraction - defines scope type and provides root scope.
 * TensorScope instance is found via implicit search, not stored here.
 */
trait TensorPlatform {
  type Scope
  def rootScope(): Scope
}

// Convenience methods in TensorScope companion
object TensorScope {
  /** Get root scope from implicit platform */
  def global[S](using p: TensorPlatform { type Scope = S }): S =
    p.rootScope()

  /** Execute in a managed global scope */
  def withGlobalScope[S: TensorScope, A](f: S => A)(
    using p: TensorPlatform { type Scope = S }
  ): A = {
    val root = p.rootScope()
    try summon[TensorScope[S]].withScope(root)(f)
    finally summon[TensorScope[S]].close(root)
  }
}

// Extension methods for cleaner syntax
extension [S: TensorScope](scope: S)
  def withSubScope[A](f: S => A): A = summon[TensorScope[S]].withScope(scope)(f)

extension [A, S: TensorScope](a: A)(using repr: TensorRepresentation[A, S])
  def toTensor(scope: S): repr.Tensor = repr.toTensor(a, scope)
```

### JVM/DJL Implementation

```scala
// jvm/src/main/scala/cps/learning/backends/djl/DJLTensorScope.scala

import ai.djl.ndarray.{NDArray, NDManager}
import ai.djl.ndarray.types.Shape

// Platform implementation
object DJL extends TensorPlatform {
  type Scope = NDManager
  def rootScope(): NDManager = NDManager.newBaseManager(Device.gpu())
}

// Type class instances - found via implicit search
given TensorScope[NDManager] with {
  def subScope(m: NDManager): NDManager = m.newSubManager()
  def close(m: NDManager): Unit = m.close()
}

given TensorRepresentation[Board, NDManager] with {
  type Tensor = NDArray

  def toTensor(a: Board, scope: NDManager): NDArray = {
    val size = a.size
    val arr = new Array[Float](size * size)
    for {
      i <- 0 until size
      j <- 0 until size
    } arr(i * size + j) = a.getOrElse((i, j), 0).toFloat
    scope.create(arr, new Shape(size, size))
  }

  def fromTensor(t: NDArray): Option[Board] = {
    // ... implementation
  }

  def tensorShape(a: Board): Seq[Int] = Seq(a.size, a.size)
}
```

### Usage

```scala
// Using platform to get root scope
given TensorPlatform = DJL

val root = TensorScope.global
root.withSubScope { scope =>
  val tensor = board.toTensor(scope)
  // ... use tensor
}
// scope closed, tensors freed

// Or use convenience method for fully managed scope
TensorScope.withGlobalScope { scope =>
  board.toTensor(scope)
}

// Platform-generic code
def train[S: TensorScope](using
  p: TensorPlatform { type Scope = S },
  repr: TensorRepresentation[Board, S]
): Unit = {
  TensorScope.withGlobalScope { scope =>
    board.toTensor(scope)
  }
}

// In DJLRLModelControl
class DJLRLModelControl[F[_], S, O, A](params: DJLRLModelParams)(
  using obsRepr: TensorRepresentation[O, NDManager]
) {
  def rateActions(modelState: DJRLModelState[O, A], ...) = {
    modelState.qNetworkTrainer.getManager.withSubScope { scope =>
      val input = observation.toTensor(scope)
      val qValues = modelState.qNetworkPredictor.predict(input)
      // ...
    }
  }
}
```

## Benefits

1. **Explicit Lifecycle**: Context parameter makes resource management visible in the type signature - can't create tensor without specifying manager.

2. **Scoped Cleanup**: `withScope` ensures all tensors are cleaned up when scope exits, even on exceptions.

3. **Platform Parameterized**: Same pattern works for different backends:
   - JVM/DJL: `NDManager`
   - JS/WebGL: `WebGLContext` (future)
   - Native/CUDA: `CudaStream` (future)

4. **Composable**: Scopes can be nested for fine-grained memory management.

## Migration Path

1. Add new traits alongside existing code
2. Update `DJLRLModelControl` to use new pattern
3. Update `TensorRepresentation[Board]` implementation
4. Remove old `given (using NDManager): TensorRepresentation[Board]`
5. Clean up any remaining `attach()` calls

## Batching

### Problem

Neural networks process batches efficiently (GPU parallelism, memory bandwidth). Current code does manual batching with multiple copies:

```scala
// Current inefficient approach in trainBatch
for (i <- sample.indices) {
  val obsTensor = obsRepr.toTensor(exp.observation)  // Create tensor
  obsTensor.attach(subManager)
  val obsArray = obsTensor.toFloatArray              // Copy to CPU array
  System.arraycopy(obsArray, 0, obsData, i * size, size)  // Copy to batch array
}
val batchedObs = subManager.create(obsData, new Shape(batchSize, obsSize))  // Create batch tensor
```

This has 3 copies per observation: domain → tensor → float[] → batched tensor.

### Why Not Extend TensorRepresentation?

Adding `toBatchedTensor` to base trait isn't always possible:

1. **Variable-sized inputs**: If `Board` can be 3x3 or 5x5, can't batch different sizes without padding
2. **Irregular structures**: Graphs with different node counts, variable-length sequences
3. **No meaningful batch dimension**: Some data doesn't naturally batch

### Proposed: Optional BatchableTensorRepresentation

```scala
// Core type class - always available
trait TensorRepresentation[A, S: TensorScope] {
  type Tensor
  def toTensor(a: A, scope: S): Tensor
  def fromTensor(t: Tensor): Option[A]
  def tensorShape(a: A): Seq[Int]
}

// Optional extension - only for fixed-size, batchable types
trait BatchableTensorRepresentation[A, S: TensorScope] extends TensorRepresentation[A, S] {
  /** Fixed shape required for batching */
  def fixedShape: Seq[Int]

  /** Direct batched conversion - single copy */
  def toBatchedTensor(batch: IndexedSeq[A], scope: S): Tensor

  def fromBatchedTensor(t: Tensor): IndexedSeq[A]
}
```

### JVM Implementation

```scala
given BatchableTensorRepresentation[Board, NDManager] with {
  type Tensor = NDArray

  def fixedShape: Seq[Int] = Seq(boardSize, boardSize)

  def toTensor(a: Board, manager: NDManager): NDArray = {
    val arr = new Array[Float](boardSize * boardSize)
    fillBoardData(a, arr, 0)
    manager.create(arr, new Shape(boardSize, boardSize))
  }

  def toBatchedTensor(batch: IndexedSeq[Board], manager: NDManager): NDArray = {
    val batchSize = batch.size
    val obsSize = boardSize * boardSize
    val arr = new Array[Float](batchSize * obsSize)

    for (i <- batch.indices) {
      fillBoardData(batch(i), arr, i * obsSize)  // Write directly to batch array
    }
    manager.create(arr, new Shape(batchSize, obsSize))
  }

  private def fillBoardData(board: Board, arr: Array[Float], offset: Int): Unit = {
    for {
      i <- 0 until boardSize
      j <- 0 until boardSize
    } arr(offset + i * boardSize + j) = board.getOrElse((i, j), 0).toFloat
  }

  // ... fromTensor, fromBatchedTensor
}
```

### Usage in Model Control

```scala
class DJLRLModelControl[F[_], S, O, A](params: DJLRLModelParams)(
  using
    scopeTC: TensorScope[NDManager],
    obsRepr: BatchableTensorRepresentation[O, NDManager]  // Requires batchable
) {

  def trainBatch(modelState: DJRLModelState[O, A], buffer: Vector[Experience[O, A]]) = {
    scopeTC.withScope(modelState.qNetworkTrainer.getManager) { scope =>
      // Direct batched conversion - single copy per batch
      val batchedObs = obsRepr.toBatchedTensor(buffer.map(_.observation), scope)
      val batchedNextObs = obsRepr.toBatchedTensor(buffer.map(_.nextObservation), scope)

      // Efficient batch operations
      val qValues = predictor.predict(batchedObs)
      val nextQValues = targetPredictor.predict(batchedNextObs)
      // ...
    }
  }
}
```

## Resolved Questions

### Q1: How to handle tensors that need to outlive a scope (e.g., model parameters)?

**Answer**: Use the root scope from `TensorPlatform`. Model parameters are allocated in the root scope which outlives sub-scopes used for training data:

```scala
given platform: TensorPlatform = DJL

// Model parameters live in root scope
val rootScope = TensorScope.global
val modelWeights = createModel(rootScope)  // Long-lived

// Training data uses sub-scopes that get cleaned up
rootScope.withSubScope { batchScope =>
  val batchTensor = data.toTensor(batchScope)  // Cleaned up after batch
  train(modelWeights, batchTensor)
}
// batchScope closed, training tensors freed
// modelWeights still alive in rootScope
```

## Open Questions

None currently - extension methods provide sufficient convenience for current use cases.
