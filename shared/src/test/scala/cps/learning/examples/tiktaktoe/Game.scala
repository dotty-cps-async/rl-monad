package cps.learning.examples.tiktaktoe

import cps.*
import cps.learning.*
import cps.monads.logic.{*, given}

case class GameState(board: Board, nextPlayer: Int)

class TikTakToeGame(boardSize: Int, n: Int) extends RLEnvironment[GameState, Move] {

  def initState: GameState = GameState(Board.empty(boardSize), 1)

  override def isFinalState(state: GameState): Boolean =
    state.board.winner.isDefined || state.board.isFull

  def applyAction(state: GameState, action: Move): Option[(GameState, Float)] =
    if !isActionPossible(state, action) then
      None
    else
      val newState = state.board.update(action.i, action.j, action.player)
      val newPlayer = flipPlayer(action.player)
      val reward = newState.winner match
        case Some(p) =>
          if p == action.player then 1.0f else -1.0f
        case None =>
          if newState.isFull then -1.0f else 0.0f
      Some((GameState(newState, newPlayer), reward))

  def flipPlayer(i: Int): Int =
    if (i == 1) 2 else 1

  def possibleActions[F[_] : CpsFloatOrderedLogicMonad](state: GameState): F[Move] = {
    val moves = for {
      i <- 0 until boardSize
      j <- 0 until boardSize
      if state.board.isEmpty(i, j)
    } yield Move(i, j, state.nextPlayer)
    all(moves)
  }

  def isActionPossible(state: GameState, action: Move): Boolean = {
    state.nextPlayer == action.player &&
      !state.board.isBusy(action.i, action.j) &&
      action.i >= 0 && action.i < boardSize &&
      action.j >= 0 && action.j < boardSize
  }

}