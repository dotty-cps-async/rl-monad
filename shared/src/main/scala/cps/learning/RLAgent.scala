package cps.learning

import cps.*
import cps.monads.logic.*

enum AgentRunningMode {
  case Explore, Exploit
}

trait RLAgentBehavior[F[_] : CpsFloatOrderedLogicMonad, S, A] {

  type AgentState

  def chooseAction(env: RLEnvironment[S, A], envState: S, agentState: AgentState, mode: AgentRunningMode): F[A]

  def performStep(env: RLEnvironment[S, A], envState: S, agentState: AgentState, mode: AgentRunningMode): F[(Option[S], AgentState)]

}



trait RLAgent[F[_] : CpsFloatOrderedLogicMonad, S, A] {

  type AgentState

  def chooseAction(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[A]
  
}


trait RLImmutableAgent[F[_] : CpsFloatOrderedLogicMonad, S, A] extends RLAgent[F, S, A] {

  type Self <: RLImmutableAgent[F, S, A]

  type AgentStep = Self

  def step(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[(Self, Option[S])]

}


trait RLMutableAgent[F[_] : CpsFloatOrderedLogicMonad, S, A] extends RLAgent[F, S, A] {

  type Self = this.type

  type AgentState = Self

  /**
   * Perform step in environment, choose action and change own state.
   */
  def step(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[Option[S]]

}


class RLImmutableModelAgent[F[_] : CpsFloatOrderedLogicMonad, M, S, A](model: RLImmutableModel[S, A, Float]) extends RLImmutableAgent[F, S, A] {

  type Self = RLImmutableModelAgent[F, M, S, A]

  
  def chooseAction(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[A] =
    reify[F] {
      val possibleActions = env.possibleActions[F](state)
      // TODO: maybe in explore mode, do exploration with exloration rate more than epsilon-greedy
      val actionSet = fromObserver(possibleActions.observeN(model.maxPossibleAction)).reflect
      model.selectOne(state, actionSet, mode)
    }

  override def step(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[(Self, Option[S])] = reify[F] {
    if env.isFinalState(state) then
      // If state is final, we do not need to choose action and just return the same state
      RLImmutableModelAgent(model) -> None
    else
      val a = chooseAction(env, state, mode).reflect
      env.applyAction(state, a) match
        case Some((newState, reward)) =>
          val finish = env.isFinalState(newState)
          val nextModel = model.trainCase(state, newState, a, reward, finish)
          RLImmutableModelAgent(nextModel) -> Some(newState)
        case None =>
          val nextModel = model.trainCase(state, state, a, RLModel.BIG_NEGATIVE, false)
          RLImmutableModelAgent(nextModel) -> None
  }

}

class RLMutableModelAgent[F[_] : CpsFloatOrderedLogicMonad, M, S, A](model: RLMutableModel[S, A, Float]) extends RLMutableAgent[F, S, A] {

  
  def chooseAction(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[A] =
    reify[F] {
      val possibleActions = env.possibleActions[F](state)
      // TODO: maybe in explore mode, do exploration with exloration rate more than epsilon-greedy
      val actionSet = fromObserver(possibleActions.observeN(model.maxPossibleAction)).reflect
      model.selectOne(state, actionSet, mode)
    }

  override def step(env: RLEnvironment[S, A], state: S, mode: AgentRunningMode): F[Option[S]] = reify[F] {
    if (env.isFinalState(state)) then None
    else
      val a = chooseAction(env, state, mode).reflect
      env.applyAction(state, a) match
        case Some((newState, reward)) =>
          val finish = env.isFinalState(newState)
          model.trainCase(state, newState, a, reward, finish)
          Some(newState)
        case None =>
          model.trainCase(state, state, a, RLModel.BIG_NEGATIVE, false)
          None
  }


}