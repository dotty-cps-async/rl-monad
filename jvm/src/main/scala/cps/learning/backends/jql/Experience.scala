package cps.learning.backends.jql

import cps.learning.*

case class Experience[S: NDArrayRepresentation, A: IntRepresentation](
                                                                       state: S,
                                                                       action: A,
                                                                       nextState: S,
                                                                       reward: Float,
                                                                       done: Boolean
                                                                     )
