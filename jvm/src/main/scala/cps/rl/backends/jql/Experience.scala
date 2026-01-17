package cps.rl.backends.jql

import ai.djl.ndarray.NDArray
import cps.rl.*

case class Experience[O, A: IntRepresentation](
                                                observation: O,
                                                action: A,
                                                nextObservation: O,
                                                reward: Float,
                                                done: Boolean
                                              )
