package cps.learning.backends.jql

import cps.learning.TensorRepresentation

trait NDListRepresentation[A] extends TensorRepresentation[A] {

  type Tensor = ai.djl.ndarray.NDList
  
  
  
}
