package cps.learning

import cps.*
import cps.learning.RLAgentStepResult
import cps.monads.logic.{*, given}


class RLBranchAndBoundsAgentBehavior[F[_] : CpsScoredLogicMonad.Curry[R], S, O, A, M, R: LinearlyOrderedGroup](modelControl: RLModelControl[F, S, O, A, R, M]) extends RLAgentBehavior[F, S, O, A, R] {

  type AgentBehaviorState = M

  inline def rlMonad: CpsScoredLogicMonad[F, R] = summon[CpsScoredLogicMonad[F, R]]

  def possibleActions(env: RLEnvironment[S, O, A], envState: S, agentState: AgentBehaviorState): IndexedSeq[A] =
    ???

  def chooseAction(env: RLEnvironment[S, O, A], envState: S, agentBehaviorState: AgentBehaviorState, mode: AgentRunningMode): F[A] =
    val actionSet = possibleActions(env, envState, agentBehaviorState)
    val observation = env.observe(envState)
    modelControl.rateActions(agentBehaviorState, observation, actionSet, mode)

  def prepareAction(agentState: AgentBehaviorState, action: A): AgentBehaviorState = agentState

  def performVirtualStep(env: RLEnvironment[S, O, A], state: S, agentBehaviorState: AgentBehaviorState, action: A, recursionCount: Int): F[RLAgentStepResult[S,M]] = {
    if env.isFinalState(state) then
      rlMonad.pure(RLAgentStepResult.Finished(state, agentBehaviorState))
    else {
      val newBehaviorState = prepareAction(agentBehaviorState, action)
      env.applyAction(state, action) match {
        case Some((newState, reward)) =>
          if env.isFinalState(newState) then
            rlMonad.pure(RLAgentStepResult.Finished(newState, newBehaviorState))
          else if recursionCount > 0 then
            val observation = env.observe(newState)
            val ratedActions = modelControl.rateActions(newBehaviorState, observation, possibleActions(env, state, newBehaviorState), AgentRunningMode.Explore)
            rlMonad.flatMap(ratedActions) { a =>
              performVirtualStep(env, newState, agentBehaviorState, a, recursionCount - 1)
            }
          else
            rlMonad.pure(RLAgentStepResult.Continued(newState, newBehaviorState))
        case None =>
          rlMonad.pure(RLAgentStepResult.InvalidAction(agentBehaviorState))
      }
    }
  }

  override def performStep(env: RLEnvironment[S, O, A], envState: S, agentBehaviorState: M, mode: AgentRunningMode): F[RLAgentStepResult[S, M]] = {
    ???

  }

}
