package cps.learning

/**
 * Model are defined by
 * - input in I
 * - output in O
 **/
trait RLModel[I,A,S] {

  def chooseAction(input:I,state:S): A

  def applyAction(action:A, state:S): S

  def reward(state:S, rw: Double): S
}
