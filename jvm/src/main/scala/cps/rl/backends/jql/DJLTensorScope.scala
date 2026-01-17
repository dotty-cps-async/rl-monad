package cps.rl.backends.jql

import ai.djl.Device
import ai.djl.ndarray.NDManager
import ai.djl.engine.Engine
import cps.rl.{TensorScope, TensorPlatform}

/**
 * TensorScope implementation for DJL's NDManager.
 * Enables automatic tensor memory management through sub-managers.
 */
given TensorScope[NDManager] with {
  def subScope(m: NDManager): NDManager = m.newSubManager()
  def close(m: NDManager): Unit = m.close()
}

/**
 * DJL platform implementation.
 * Provides root scope factory with configurable device.
 *
 * Usage:
 * {{{
 * given TensorPlatform = DJL.withAutoDevice()
 *
 * TensorScope.withGlobalScope { scope =>
 *   val tensor = board.toTensor(scope)
 *   // ... use tensor
 * }
 * }}}
 */
object DJL extends TensorPlatform {
  type Scope = NDManager

  @volatile private var _device: Device = Device.cpu()

  /**
   * Configure the device to use for tensor operations.
   * @return this for method chaining
   */
  def withDevice(device: Device): DJL.type = {
    _device = device
    this
  }

  /**
   * Get the currently configured device.
   */
  def device: Device = _device

  /**
   * Detect available GPU and use it, falling back to CPU.
   * @return the detected device
   */
  def detectDevice(): Device = {
    val engine = Engine.getInstance()
    val gpuCount = engine.getGpuCount()
    if (gpuCount > 0) Device.gpu(0) else Device.cpu()
  }

  /**
   * Configure to automatically detect and use GPU if available.
   * @return this for method chaining
   */
  def withAutoDevice(): DJL.type = withDevice(detectDevice())

  /**
   * Create a new root scope (NDManager) with the configured device.
   * The caller is responsible for closing the returned manager.
   */
  override def rootScope(): NDManager = NDManager.newBaseManager(_device)
}
