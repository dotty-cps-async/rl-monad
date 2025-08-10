package cps.learning

import cps.*
import cps.monads.logic.*
import cps.syntax.{*, given}


/**
 * Environment are defined by
 * - State S
 * - Action A
 **/
trait RLEnvironment[S, A] {

  /**
   * return initial state
   */
  def initState: S

  /**
   * return true if state is final
   */
  def isFinalState(state: S): Boolean
  
  /**
   * Apply action to state and return new state and reward if possible.
   * If applyAction is not possible, it should return None
   */
  def applyAction(state: S, action: A): Option[(S, Float)]
  
  /**
   * Return the set possible actions for a given state (potenically can be infinite),
   * but only finite subset is actually explored
   */
  def possibleActions[F[_]:CpsFloatOrderedLogicMonad](state: S): F[A]

  /**
   * check if action is possible in state
   */
  def isActionPossible(state: S, action: A): Boolean

}





