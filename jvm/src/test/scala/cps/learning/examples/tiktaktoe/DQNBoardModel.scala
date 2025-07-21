package cps.learning.examples.tiktaktoe

import ai.djl.nn.SequentialBlock
import ai.djl.Model

object DQNBoardModel {

  def build(boardSize: Int): Model = {
    val block = buildBlock(boardSize)
    Model.newInstance(block)
  }

  def buildBlock(boardSize: Int): Block = {
     val S1 = boardSize*boardSize*20
     val S2 = S1 / 2
     new SequentialBlock("tik-tak-toke-board")
       .add(Linear(boardSize * boardSize, S1))
       .add(Activation("relu"))
       .add(Linear(S1, S2))
       .add(Activation("relu"))
       .add(Linear(S2, boardSize*boardSize))
       .add(Activation("sigmoid"))
  }

}