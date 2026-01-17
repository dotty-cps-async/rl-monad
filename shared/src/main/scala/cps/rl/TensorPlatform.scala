package cps.rl

/**
 * Platform abstraction for tensor operations.
 * Defines the scope type and provides a factory for root scopes.
 *
 * Implementations provide platform-specific scope management:
 * - JVM/DJL: Scope = NDManager
 * - JS/WebGL: Scope = WebGLContext (future)
 * - Native/CUDA: Scope = CudaStream (future)
 *
 * The TensorScope instance for the Scope type is found via implicit search,
 * allowing different behaviors without modifying this trait.
 *
 * Example usage:
 * {{{
 * given TensorPlatform = DJL.withDevice(Device.gpu())
 *
 * TensorScope.withGlobalScope { scope =>
 *   val tensor = board.toTensor(scope)
 *   // ... use tensor
 * }
 * // scope closed, tensors freed
 * }}}
 */
trait TensorPlatform {
  /**
   * The scope type for this platform.
   * For DJL this would be NDManager.
   */
  type Scope

  /**
   * Create a new root scope.
   * The caller is responsible for closing the root scope.
   */
  def rootScope(): Scope
}
