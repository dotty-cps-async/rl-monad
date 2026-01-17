# rl-logic

A monadic framework for reinforcement learning built on top of [dotty-cps-async](https://github.com/rssh/dotty-cps-async).

## Package

```scala
import cps.rl.*
```

## Features

- **Scored Logic Monad** - Monadic abstraction for weighted search with scores (`CpsScoredLogicMonad`, `CpsOrderedLogicMonad`)
- **RL Abstractions** - Core traits for environments, agents, and models (`RLEnvironment`, `RLAgentBehavior`, `RLModelControl`)
- **Tensor Support** - Type-safe tensor operations with scope management (`TensorType`, `TensorScope`, `TensorRepresentation`)
- **Priority Queue Data Structures** - Efficient heap implementations including `PairingHeap`, `FingerTree`, and scaled variants
- **Cross-Platform** - Supports JVM, Scala.js, and Scala Native

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.dotty-cps-async" %%% "rl-logic" % "<version>"
```

## Dependencies

- Scala 3.3.7+
- [dotty-cps-async](https://github.com/rssh/dotty-cps-async) 1.2.0
- [DJL (Deep Java Library)](https://djl.ai/) for neural network support (JVM only)

## License

Apache 2.0
