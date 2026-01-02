package cps.learning

import cps.*


/**
 * General interface for a Reinforcement Learning model, which exposes part of the functinality used by the Agent and Environment.
 */
trait RLModelControl[F[_] : CpsScoredLogicMonad.Curry[R], S, A, R: LinearlyOrderedGroup, M] {

  /**
   * Create initial model
   */
  def initialModel: M

  /**
   * Maximum number of possible actions (i.e. size of action space)
   */
  def maxPossibleActons(model: M): Int
  
  /**
   * Select the best action for a given state
   */
  def rateActions(model: M, input: S, actions: IndexedSeq[A], mode: AgentRunningMode): F[A]

  /**
   * Update the model with a new state, action and reward.
   */
  def trainCase(model: M, state: S, nextState: S, action: A, reward: Float, finsih:  Boolean): F[M]

}

object RLModelControl {

  /**
   * Default value of the big negative reward.
   */
  val BIG_NEGATIVE = -1000000.0f

  /**
   * Default value of the big positive reward.
   */
  val BIG_POSITIVE = 1000000.0f


}

