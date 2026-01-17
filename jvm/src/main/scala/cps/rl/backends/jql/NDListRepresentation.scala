package cps.rl.backends.jql

import cps.rl.TensorRepresentation

trait NDListRepresentation[A] extends TensorRepresentation[A] {

  type Tensor = ai.djl.ndarray.NDList
  
  
  
}
