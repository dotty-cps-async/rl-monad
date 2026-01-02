package cps.learning


/**
 * RLLgicEnvironment is RL environment defined which can produce multiple reactions with scores in logic monad.
 * (Example -- representation of stochastic environments or 'other player' with multiple possible reactions)
 *
 * @param cpsScoredLogicMonad$F$0
 * @tparam F
 */
trait RLLogicEnvironment[F[_] : CpsScoredLogicMonad.Curry[R], S, A, R] {

  /**
   * return possibke initial states
   */
  def initState: S

  /**
   * return true if state is final
   */
  def isFinalState(state: S): Boolean

  /**
   * Apply action to state and return new state and reward if possible.
   * If applyAction is not possible, it should return None
   * Since this is logic environment, multiple possible results with scores can be returned in scored logic monad.
   */
  def applyActions(state: S, action: A): F[Option[(S, Float)]]


  /**
   * check if action is possible in state
   */
  def isActionPossible(state: S, action: A): Boolean


}
