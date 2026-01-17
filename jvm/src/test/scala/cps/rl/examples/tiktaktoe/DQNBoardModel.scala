package cps.rl.examples.tiktaktoe

import ai.djl.nn.Block
import ai.djl.nn.SequentialBlock
import ai.djl.nn.core.Linear
import ai.djl.nn.convolutional.Conv2d
import ai.djl.nn.Activation
import ai.djl.nn.Blocks
import ai.djl.Model
import ai.djl.ndarray.NDManager
import ai.djl.ndarray.types.Shape
import ai.djl.ndarray.types.DataType


object DQNBoardModel {

  def build(boardSize: Int): Model = {
    val observationSize = 2 * boardSize * boardSize  // relative encoding
    build(boardSize, observationSize)
  }

  def build(boardSize: Int, observationSize: Int): Model = {
    val actionSize = boardSize * boardSize
    val block = buildBlock(observationSize, actionSize)
    val model = Model.newInstance("tik-tak-toke-model")
    model.setBlock(block)
    val inputShape = new Shape(1, observationSize)

    val ndManager = NDManager.newBaseManager()
    try
      block.initialize(ndManager, DataType.FLOAT32, inputShape)
    finally
      ndManager.close()

    model
  }

  /**
   * Build DQN network block.
   *
   * @param observationSize Input size (e.g., 50 for 5x5 board with 2-channel relative encoding)
   * @param actionSize Output size (e.g., 25 for 5x5 board)
   */
  def buildBlock(observationSize: Int, actionSize: Int): Block = {
    val S1 = observationSize * 4  // Hidden layer 1
    val S2 = S1 / 2               // Hidden layer 2
    new SequentialBlock()
      .add(Linear.builder.setUnits(S1).build())
      .add(Activation.reluBlock())
      .add(Linear.builder.setUnits(S2).build())
      .add(Activation.reluBlock())
      .add(Linear.builder.setUnits(actionSize).build())
      // No activation - Q-values should be unbounded
  }

  /** Legacy method for backward compatibility */
  def buildBlock(boardSize: Int): Block = {
    val observationSize = 2 * boardSize * boardSize
    val actionSize = boardSize * boardSize
    buildBlock(observationSize, actionSize)
  }

  /**
   * Build CNN-based DQN network block.
   *
   * Input shape: (batch, 2, boardSize, boardSize) - 2 channels for my_pieces and opponent_pieces
   * Output shape: (batch, actionSize) - Q-values for each board position
   *
   * Architecture:
   *   Conv2d(2→32, 3×3, pad=1) → ReLU
   *   Conv2d(32→64, 3×3, pad=1) → ReLU
   *   Flatten
   *   Linear(64*boardSize*boardSize, 256) → ReLU
   *   Linear(256, actionSize)
   *
   * @param boardSize Board dimension (e.g., 5 for 5x5)
   * @param channels Number of input channels (default 2 for relative encoding)
   */
  def buildCNNBlock(boardSize: Int, channels: Int = 2): Block = {
    val actionSize = boardSize * boardSize
    val conv1Filters = 32
    val conv2Filters = 64
    val flattenedSize = conv2Filters * boardSize * boardSize
    val hiddenSize = 256

    new SequentialBlock()
      // Conv layer 1: (batch, 2, H, W) → (batch, 32, H, W)
      .add(Conv2d.builder()
        .setKernelShape(new Shape(3, 3))
        .optPadding(new Shape(1, 1))
        .setFilters(conv1Filters)
        .build())
      .add(Activation.reluBlock())
      // Conv layer 2: (batch, 32, H, W) → (batch, 64, H, W)
      .add(Conv2d.builder()
        .setKernelShape(new Shape(3, 3))
        .optPadding(new Shape(1, 1))
        .setFilters(conv2Filters)
        .build())
      .add(Activation.reluBlock())
      // Flatten: (batch, 64, H, W) → (batch, 64*H*W)
      .add(Blocks.batchFlattenBlock())
      // Fully connected layers
      .add(Linear.builder().setUnits(hiddenSize).build())
      .add(Activation.reluBlock())
      .add(Linear.builder().setUnits(actionSize).build())
      // No activation - Q-values should be unbounded
  }

}