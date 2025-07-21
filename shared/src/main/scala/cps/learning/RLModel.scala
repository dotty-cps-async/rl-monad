package cps.learning

import cps.*

/**
 * General interface for a Reinforcement Learning model, which exposes part of the functinality used by the Agent and Environment.
 *
 */
trait RLModel[S,A,R]  {
  
  def maxPossibleAction: Int
  
  /**
   * Predict q-value for a given state and action.
   */
  def predictQ(input: S, action:A): R

  /**
   * Select the best action for a given state
   * 
   * @param input
   * @param actions
   * @param mode
   * @return
   */
  def selectOne(input: S, actions: IndexedSeq[A], mode:AgentRunningMode): A

  /**
   * Update the model with a new state, action and reward.
   * Its automatically saved in the model and used for training when number of experiments to batch is reached.
   * @param input
   * @param target
   * @param reward
   */
  def trainCase(state: S, nextState: S, target: A, reward: R): RLModel[S,A,R]
  
  
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
 

