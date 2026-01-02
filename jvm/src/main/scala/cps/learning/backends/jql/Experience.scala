package cps.learning.backends.jql

import ai.djl.ndarray.NDArray
import cps.learning.*

case class Experience[O, A: IntRepresentation](
                                                observation: O,
                                                action: A,
                                                nextObservation: O,
                                                reward: Float,
                                                done: Boolean
                                              )
