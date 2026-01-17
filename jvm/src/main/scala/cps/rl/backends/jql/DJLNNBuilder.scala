package cps.rl.backends.jql

import ai.djl.nn.Block

@FunctionalInterface
trait DJLNNBuilder {

  def apply(): Block
  
}
