## RL-Logic: Mixing Machine Learning into Logical Search

### Core Idea

The library extends logical programming (backtracking search) with the ability to **score and reorder** computation branches. This bridges two paradigms:
- **Logical search**: declarative exploration of solution space
- **Machine learning**: provides scores/preferences to guide that search

### Programming Model

Write declarative code using direct-style syntax:
- Define alternative branches that the system explores lazily
- Insert **scoring points** where ML models can influence which branches to explore first
- The runtime handles backtracking and branch selection based on scores

### Two Ways to Score

1. **Post-computation ordering**: Evaluate several results, then reorder by score (useful when scoring depends on computed values)

2. **Pre-scored branches**: Assign scores upfront before computation (useful for "branch and bound" style algorithms where you want to prune early)

### RL Integration Pattern

The core abstraction is `RLEnvironment[S, O, A]` with three type parameters:
- **S** (State): Full game/environment state for logic (e.g., board + whose turn)
- **O** (Observation): What the neural network sees (e.g., just the board)
- **A** (Action): Available actions

```scala
trait RLEnvironment[S, O, A] {
  def observe(state: S): O      // Convert state to NN input
  def initState: S
  def isFinalState(state: S): Boolean
  def applyAction(state: S, action: A): Option[(S, Float)]
}
```

Supporting components:
1. **Agent** (`RLAgentBehavior`): Chooses actions using the scored logic monad
2. **Model** (`RLModelControl`): Handles action selection and training
3. **TensorRepresentation[O]**: Maps observations to tensors

### Backend Integration via Type Refinement

Instead of separate traits per backend, use Scala type refinement:

```scala
// Generic interface
trait TensorRepresentation[A] {
  type Tensor
  def toTensor(a: A): Tensor
}

// DJL backend requires specific tensor type
class DJLRLModelControl[...](using TensorRepresentation[O] { type Tensor = NDArray })
```

This allows a single implementation to satisfy both generic and backend-specific requirements.

### Example: Tic-Tac-Toe with Self-Play

The library includes a complete example in `jvm/src/test/scala/cps/learning/examples/tiktaktoe/`:

```scala
// Game environment with S=GameState, O=Board, A=Move
class TikTakToeGame(boardSize: Int, winLength: Int)
    extends RLEnvironment[GameState, Board, Move] {
  def observe(state: GameState): Board = state.board
  // ...
}

// Tensor representation with NDArray refinement
given TensorRepresentation[Board] with {
  type Tensor = NDArray
  def toTensor(b: Board): NDArray = ...
}
```

The `SelfPlayTrainer` runs DQN training where a single model plays both sides.

### Typical Use Cases

- **Combinatorial optimization** (shortest path, scheduling)
- **Compiler optimizations** (choosing between equivalent representations)
- **Game playing** (board games with discrete moves)
- Any problem where heuristic search can be improved by learned preferences
