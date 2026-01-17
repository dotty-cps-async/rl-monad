package cps.rl

/**
 * Type class for tensor memory scope management.
 * Platform-independent abstraction for resource lifecycle.
 *
 * Implementations manage the lifecycle of tensor memory:
 * - Creating sub-scopes for temporary allocations
 * - Closing scopes to free associated tensors
 *
 * Example usage with DJL:
 * {{{
 * given TensorScope[NDManager] with {
 *   def subScope(m: NDManager): NDManager = m.newSubManager()
 *   def close(m: NDManager): Unit = m.close()
 * }
 * }}}
 */
trait TensorScope[S] {
  /**
   * Create a new sub-scope from the parent scope.
   * Tensors created in the sub-scope will be freed when it's closed.
   */
  def subScope(s: S): S

  /**
   * Close the scope, freeing all associated tensor memory.
   */
  def close(s: S): Unit

  /**
   * Execute a function within a managed sub-scope.
   * The sub-scope is automatically closed when the function completes.
   */
  def withScope[A](parent: S)(f: S => A): A = {
    val sub = subScope(parent)
    try f(sub)
    finally close(sub)
  }
}

object TensorScope {
  /**
   * Get the root scope from the implicit platform.
   */
  def global[S](using p: TensorPlatform { type Scope = S }): S =
    p.rootScope()

  /**
   * Execute a function within a fully managed scope.
   * Creates a root scope, executes in a sub-scope, then closes both.
   */
  def withGlobalScope[S: TensorScope, A](f: S => A)(
    using p: TensorPlatform { type Scope = S }
  ): A = {
    val root = p.rootScope()
    try summon[TensorScope[S]].withScope(root)(f)
    finally summon[TensorScope[S]].close(root)
  }
}

/**
 * Extension methods for cleaner syntax when working with scopes.
 */
extension [S: TensorScope](scope: S)
  /**
   * Execute a function within a sub-scope of this scope.
   */
  def withSubScope[A](f: S => A): A = summon[TensorScope[S]].withScope(scope)(f)
