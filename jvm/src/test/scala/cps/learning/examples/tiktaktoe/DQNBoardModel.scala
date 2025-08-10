package cps.learning.examples.tiktaktoe

import ai.djl.nn.Block
import ai.djl.nn.SequentialBlock
import ai.djl.nn.core.Linear
import ai.djl.nn.Activation
import ai.djl.Model
import ai.djl.ndarray.NDManager
import ai.djl.ndarray.types.Shape
import ai.djl.ndarray.types.DataType


object DQNBoardModel {

  def build(boardSize: Int): Model = {
    val block = buildBlock(boardSize)
    val model = Model.newInstance("tik-tak-toke-model")
    model.setBlock(block)
    val inputShape = new Shape(1, boardSize * boardSize)

    val ndManager = NDManager.newBaseManager()
    try
      block.initialize(ndManager, DataType.FLOAT32, inputShape)
    finally
      ndManager.close()

    model
  }

  def buildBlock(boardSize: Int): Block = {
    val S1 = boardSize * boardSize * 20
    val S2 = S1 / 2
    new SequentialBlock()
      .add(Linear.builder.setUnits(S1).build())
      .add(Activation.reluBlock())
      .add(Linear.builder.setUnits(S2).build())
      .add(Activation.reluBlock())
      .add(Linear.builder.setUnits(boardSize * boardSize).build())
      .add(Activation.sigmoidBlock())
  }

}