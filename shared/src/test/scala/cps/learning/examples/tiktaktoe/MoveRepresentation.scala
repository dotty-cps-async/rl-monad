package cps.learning.examples.tiktaktoe

import cps.learning.IntRepresentation

class MoveIntRepresentation(boardSize: Int) extends IntRepresentation[Move] {

  def toTensor(m: Move): Int = m.i * boardSize + m.j

  def fromTensor(idx: Int): Option[Move] =
    if idx >= 0 && idx < boardSize * boardSize then
      Some(Move(idx / boardSize, idx % boardSize, 0)) // player filled at runtime
    else
      None

}

object MoveIntRepresentation {
  def apply(boardSize: Int): MoveIntRepresentation = new MoveIntRepresentation(boardSize)
}
