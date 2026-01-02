package cps.learning.backends.jql

import scala.util.Random

case class DJLRLModelParams(
                             name: String,
                             qBuilder: DJLNNBuilder,
                             stateSize: Int,
                             actionSize: Int,
                             epsilon: Float = 0.1f,
                             gamma: Float = 0.99f,
                             minBatchSize: Int = 100,
                             nStepsBetweenTraining: Int = 10,
                             targetUpdateFrequency: Int = 100,
                             random: Random,
                           )



