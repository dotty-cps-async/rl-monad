package cps.learning.backends.jql

import scala.util.Random

case class DJLRLModelParams(
                             stateSize: Int,
                             actionSize: Int,
                             epsilon: Float = 0.1f,
                             minBatchSize: Int = 100,
                             nStepsBetweenTraining: Int = 10,
                             random: Random,
                           )



