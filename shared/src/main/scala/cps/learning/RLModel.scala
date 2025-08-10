package cps.learning

import cps.*


/**
 * General interface for a Reinforcement Learning model, which exposes part of the functinality used by the Agent and Environment.
 */
trait RLModel[S, A, R] {

  /**
   * Get the maximum possible action value (i.e. size of the action space).
   */
  def maxPossibleAction: Int

  /**
   * Select the best action for a given state
   */
  def selectOne(input: S, actions: IndexedSeq[A], mode: AgentRunningMode): A


}


object RLModel {

  /**
   * Default value of the big negative reward.
   */
  val BIG_NEGATIVE = -1000000.0f

  /**
   * Default value of the big positive reward.
   */
  val BIG_POSITIVE = 1000000.0f

}


trait RLImmutableModel[S, A, R] extends RLModel[S, A, R] {

  /**
   * Update the model with a new state, action and reward.
   */
  def trainCase(state: S, nextState: S, action: A, reward: R, finsih: Boolean): RLImmutableModel[S, A, R]


}


trait RLMutableModel[S, A, R] extends RLModel[S, A, R] {

  /**
   * Update the model with a new state, action and reward.
   * Its automatically saved in the model and used for training when number of experiments to batch is reached.
   */
  def trainCase(state: S, nextState: S, target: A, reward: R, finish: Boolean): Unit
  
}
