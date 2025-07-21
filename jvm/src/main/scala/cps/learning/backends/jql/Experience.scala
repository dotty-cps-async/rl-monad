package cps.learning.backends.jql

case class Experience[S: NDArrayRepresentation, A:NDArrayRepresentation](
  state: S,
  action: A,
  nextState: S,
  reward: Float,
  done: Boolean
                                                                        )
