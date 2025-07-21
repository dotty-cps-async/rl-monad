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



enum AgentRunningMode {
  case Explore, Exploit
}

trait RLAgent[S, A] {

  def chooseAction[F[_] : CpsFloatOrderedLogicMonad](env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[A]

  def step[F[_] : CpsFloatOrderedLogicMonad](env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[(RLAgent[S,A],Option[S])]

}

object RLAgent {

  case class StepResult[S, A](agent: RLAgent[S, A], newState: Option[S])

}


class RLModelAgent[M, S,A](model: RLModel[S,A, Float]) extends RLAgent[S,A] {

  def fromObserver[F[_],A](using m:CpsLogicMonad[F])(fa:m.Observer[A]):F[A] =
    m.flattenObserver(
      m.observerCpsMonad.map(fa)(a => m.pure(a))
    )


  def chooseAction[F[_]:CpsFloatOrderedLogicMonad](env: RLEnvironment[S,A], state: S, mode: AgentRunningMode): F[A] =
    reify[F] {

      val possibleActions = env.possibleActions[F](state)
      // TODO: maybe in explore mode, do exploration with exloration rate more than epsilon-greedy
      val actionSet = fromObserver(possibleActions.observeN(model.maxPossibleAction)).reflect
      val nextAction = model.selectOne(state, actionSet, mode)
      ???
  }

  def step[F[_]:CpsFloatOrderedLogicMonad](env: RLEnvironment[S,A], state: S, mode: AgentRunningMode): F[(RLAgent[S,A],Option[S])] = reify[F]{
    val a = chooseAction(env, state, mode).reflect
    env.applyAction(state, a) match
        case Some((newState, reward)) =>
            val nextModel = model.trainCase(state, newState, a, reward)
            RLModelAgent(nextModel) -> newState
        case None =>
            val nextModel = model.trainCase(state, state, a, RLModel.BIG_NEGATIVE)
            RLModelAgent(nextModel) -> state
  }
  
}

trait RLAgentMonad[F[_],S,A](using CpsFloatScoredLogicMonad[F])  {



}

