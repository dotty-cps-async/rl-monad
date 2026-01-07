package cps.learning

import cps.*
import cps.monads.logic.*

enum AgentRunningMode {
  case Explore, Exploit
}

enum RLAgentStepResult[+S, +AS] {
  def agentBehaviorState: AS
  case Continued(newState: S, agentBehaviorState: AS) extends RLAgentStepResult[S, AS]
  case Finished(state: S, agentBehaviorState: AS) extends RLAgentStepResult[S, AS]
  case InvalidAction(agentBehaviorState: AS) extends RLAgentStepResult[Nothing, AS]
}

trait RLAgentBehavior[F[_], S, O, A, R](using CpsScoredLogicMonad[F, R], ScalingGroup[R], Ordering[R]) {

  type AgentBehaviorState

  def chooseAction(env: RLEnvironment[S, O, A], envState: S, agentBehaviorState: AgentBehaviorState, mode: AgentRunningMode): F[A]

  def performStep(env: RLEnvironment[S, O, A], envState: S, agentBehaviorState: AgentBehaviorState, mode: AgentRunningMode): F[RLAgentStepResult[S, AgentBehaviorState]]

}


trait RLModelAgentBehavior[F[_], M, S, O, A, R](modelControl: RLModelControl[F, S, O, A, R, M])(using rlMonad: CpsScoredLogicMonad[F, R]) extends RLAgentBehavior[F, S, O, A, R] {

  type AgentBehaviorState = M

  def possibleActions(env: RLEnvironment[S, O, A], envState: S, agentState: AgentBehaviorState): IndexedSeq[A]

  def chooseAction(env: RLEnvironment[S, O, A], envState: S, agentBehaviorState: AgentBehaviorState, mode: AgentRunningMode): F[A] = {
    val actions = possibleActions(env, envState, agentBehaviorState)
    val observation = env.observe(envState)
    modelControl.rateActions(agentBehaviorState, observation, actions, mode)
  }


  def performStep(env: RLEnvironment[S, O, A], envState: S, agentBehaviorState: AgentBehaviorState, mode: AgentRunningMode): F[RLAgentStepResult[S, AgentBehaviorState]] = reify[F] {
    if env.isFinalState(envState) then
      RLAgentStepResult.Finished(envState, agentBehaviorState)
    else
      val action = chooseAction(env, envState, agentBehaviorState, mode).reflect
      env.applyAction(envState, action) match
        case Some((newState, reward)) =>
          val finish = env.isFinalState(newState)
          val observation = env.observe(envState)
          val nextObservation = env.observe(newState)
          val nextModel = modelControl.trainCase(agentBehaviorState, observation, nextObservation, action, reward, finish).reflect
          RLAgentStepResult.Continued(newState, nextModel)
        case None =>
          val observation = env.observe(envState)
          val nextModel = modelControl.trainCase(agentBehaviorState, observation, observation, action, RLModelControl.BIG_NEGATIVE, false).reflect
          RLAgentStepResult.InvalidAction(nextModel)
  }


}


