package cps.learning

import cps.*
import cps.monads.logic.{*, given}


enum VirtualStepTrainingMode:
  case OnCommit   // Train only on the committed action (at root level)
  case OnExplore  // Train on all explored branches


/**
 * Participant in minimax search.
 * performVirtualStep does ONE step: picks action, applies it, returns result.
 * Recursive exploration is controlled by the caller.
 */
trait RLMiniMaxParticipant[F[_], S, O, A, R]:
  type ParticipantState

  /**
   * Internal action selection - just rates/picks action.
   */
  def rateAndChooseAction(env: RLEnvironment[S, O, A], state: S, participantState: ParticipantState, mode: AgentRunningMode): F[A]

  def possibleActions(env: RLEnvironment[S, O, A], state: S, participantState: ParticipantState): IndexedSeq[A]

  /**
   * Perform ONE virtual step: pick action, apply it, train if needed, return result.
   * Returns empty if all actions lead to loss.
   * recursionCount is passed for training mode decisions, not for recursive exploration.
   */
  def performVirtualStep(env: RLEnvironment[S, O, A], state: S, participantState: ParticipantState, recursionCount: Int): F[RLAgentStepResult[S, ParticipantState]]


/**
 * MiniMax agent behavior for two-player adversarial games.
 * Symmetric self-play: same agent logic plays both sides.
 * Agent only tracks its own state M (no separate opponent state).
 *
 * performVirtualStep does ONE step: picks action, applies it, returns result.
 * Recursive exploration is for filtering/scoring via the scored logic monad.
 *
 * @tparam F Effect type (scored logic monad)
 * @tparam S Environment state
 * @tparam O Observation type
 * @tparam A Action type
 * @tparam M Model/agent state (same for both players in symmetric games)
 * @tparam R Score type
 */
trait RLMiniMaxAgentBehavior[F[_] : CpsScoredLogicMonad.Curry[R], S, O, A, M, R: LinearlyOrderedGroup](
    modelControl: RLModelControl[F, S, O, A, R, M],
    maxRecursionDepth: Int,
    trainingMode: VirtualStepTrainingMode = VirtualStepTrainingMode.OnCommit
) extends RLAgentBehavior[F, S, O, A, R] with RLMiniMaxParticipant[F, S, O, A, R] {

  type AgentBehaviorState = M
  type ParticipantState = M

  inline def rlMonad: CpsScoredLogicMonad[F, R] = summon[CpsScoredLogicMonad[F, R]]

  def possibleActions(env: RLEnvironment[S, O, A], state: S, agentState: AgentBehaviorState): IndexedSeq[A]

  /**
   * Estimate the gain for the current state.
   * Return 'one' (multiplicative identity) when gain is unknown - preserves action rating.
   * Scores combine multiplicatively: finalScore = actionScore * gain.
   */
  def gainEstimation(env: RLEnvironment[S, O, A], state: S, reward: Float): R

  /**
   * Threshold below which we consider the position a loss.
   * Branches with gain < lossThreshold are pruned via empty.
   */
  def lossThreshold: R

  private def shouldTrain(recursionCount: Int): Boolean = trainingMode match
    case VirtualStepTrainingMode.OnExplore => true
    case VirtualStepTrainingMode.OnCommit => recursionCount == maxRecursionDepth

  def rateAndChooseAction(env: RLEnvironment[S, O, A], envState: S, agentState: AgentBehaviorState, mode: AgentRunningMode): F[A] =
    val actionSet = possibleActions(env, envState, agentState)
    val observation = env.observe(envState)
    modelControl.rateActions(agentState, observation, actionSet, mode)

  /**
   * Explore using minimax and apply the chosen action.
   * Self-recursive for symmetric games: same logic plays both sides.
   * Returns result after OUR one step, not after full recursive exploration.
   */
  private def exploreAndApplyAction(env: RLEnvironment[S, O, A], state: S, agentState: AgentBehaviorState, recursionCount: Int): F[(A, RLAgentStepResult[S, AgentBehaviorState])] = {
    val ordering = summon[LinearlyOrderedGroup[R]]
    val actionF = rateAndChooseAction(env, state, agentState, AgentRunningMode.Explore)
    rlMonad.flatMap(actionF) { action =>
      val observation = env.observe(state)
      env.applyAction(state, action) match {
        case Some((newState, reward)) =>
          val gain = gainEstimation(env, newState, reward)
          if ordering.lt(gain, lossThreshold) then
            val nextObservation = env.observe(newState)
            if shouldTrain(recursionCount) then
              rlMonad.flatMap(modelControl.trainCase(agentState, observation, nextObservation, action, reward, true))(_ => rlMonad.empty)
            else
              rlMonad.empty
          else
            val finish = env.isFinalState(newState)
            val nextObservation = env.observe(newState)
            val trainedStateF =
              if shouldTrain(recursionCount) then
                modelControl.trainCase(agentState, observation, nextObservation, action, reward, finish)
              else
                rlMonad.pure(agentState)
            rlMonad.flatMap(trainedStateF) { trainedState =>
              if finish then
                rlMonad.scoredPure((action, RLAgentStepResult.Finished(newState, trainedState)), gain)
              else if recursionCount > 1 then
                // Self-recursive: same agent plays opponent's turn
                // Explore ALL opponent responses for proper minimax
                val opponentResultF = performVirtualStep(env, newState, trainedState, recursionCount - 1)
                // flatMap explores all opponent moves; otherwise handles empty (opponent has no valid moves = we win)
                rlMonad.flatMap(opponentResultF) { opponentResult =>
                  rlMonad.scoredPure((action, RLAgentStepResult.Continued(newState, opponentResult.agentBehaviorState)), gain)
                }.otherwise(
                  rlMonad.scoredPure((action, RLAgentStepResult.Finished(newState, trainedState)), ordering.maxPositiveValue)
                )
              else
                rlMonad.scoredPure((action, RLAgentStepResult.Continued(newState, trainedState)), gain)
            }
        case None =>
          rlMonad.empty
      }
    }
  }

  /**
   * Perform ONE virtual step with minimax exploration for filtering/scoring.
   * Returns result after our single action (not after full recursive exploration).
   */
  override def performVirtualStep(env: RLEnvironment[S, O, A], state: S, agentState: AgentBehaviorState, recursionCount: Int): F[RLAgentStepResult[S, AgentBehaviorState]] =
    if env.isFinalState(state) then
      rlMonad.pure(RLAgentStepResult.Finished(state, agentState))
    else
      rlMonad.map(exploreAndApplyAction(env, state, agentState, recursionCount))(_._2)

  override def chooseAction(env: RLEnvironment[S, O, A], state: S, agentState: AgentBehaviorState, mode: AgentRunningMode): F[A] =
    rlMonad.map(exploreAndApplyAction(env, state, agentState, maxRecursionDepth))(_._1)

  override def performStep(env: RLEnvironment[S, O, A], envState: S, agentState: AgentBehaviorState, mode: AgentRunningMode): F[RLAgentStepResult[S, AgentBehaviorState]] =
    performVirtualStep(env, envState, agentState, maxRecursionDepth)

}
