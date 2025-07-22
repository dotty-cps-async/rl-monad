package cps.learning

import cps.*
import cps.monads.logic.*
import cps.syntax.{*, given}

import scala.util.Random

/**
 * Environment are defined by
 * - State S
 * - Action A
 **/
trait RLEnvironment[S, A] {

  /**
   * return initial state
   * @return
   */
  def initState: S

  /**
   * return true if state is final
   * @param state
   * @return
   */
  def isFinalState(state: S): Boolean
  
  /**
   * Apply action to state and return new state and reward if possible.
   * If applyAction is not possible, it should return None
   * @param state
   * @param action
   * @return
   */
  def applyAction(state: S, action: A): Option[(S, Float)]
  
  /**
   * Return the set possible actions for a given state (potenically can be infinite),
   * but only finite subset is actually explored
   * @param state
   * @return
   */
  def possibleActions[F[_]:CpsFloatOrderedLogicMonad](state: S): F[A]

  /**
   * check if action is possible in state
   */
  def isActionPossible(state: S, action: A): Boolean

}





