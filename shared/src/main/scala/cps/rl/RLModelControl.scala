package cps.rl

import cps.*


/**
 * General interface for a Reinforcement Learning model, which exposes part of the functionality used by the Agent and Environment.
 *
 * Type parameters:
 * - F[_]: Effect type (logic monad)
 * - S: State type (full game state)
 * - O: Observation type (what neural network sees)
 * - A: Action type
 * - R: Reward/score type
 * - M: Model state type
 */
trait RLModelControl[F[_] : CpsScoredLogicMonad.Curry[R], S, O, A, R: ScalingGroup : Ordering, M] {

  /**
   * Create initial model
   */
  def initialModel: M

  /**
   * Maximum number of possible actions (i.e. size of action space)
   */
  def maxPossibleActions(model: M): Int

  /**
   * Select the best action for a given observation
   */
  def rateActions(model: M, observation: O, actions: IndexedSeq[A], mode: AgentRunningMode): F[A]

  /**
   * Update the model with observation, action and reward.
   */
  def trainCase(model: M, observation: O, nextObservation: O, action: A, reward: Float, finish: Boolean): F[M]

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

