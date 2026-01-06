package cps.learning.backends.jql

import scala.util.Random

case class DJLRLModelParams(
                             name: String,
                             qBuilder: DJLNNBuilder,
                             observationSize: Int,
                             actionSize: Int,
                             epsilon: Float = 0.1f,
                             gamma: Float = 0.99f,
                             minBatchSize: Int = 100,
                             nStepsBetweenTraining: Int = 10,
                             targetUpdateFrequency: Int = 100,
                             random: Random,
                             // Optional custom input shape for CNN models
                             // If None, uses (batchSize, observationSize)
                             // For CNN, use Some(Seq(channels, height, width)) e.g. Some(Seq(2, 5, 5))
                             inputShape: Option[Seq[Int]] = None
                           )



