package cps.learning.backends.jql

import cps.learning.TensorRepresentation

class NDListRepresentation[A] extends TensorRepresentation[A] {

  type Tensor = ai.djl.ndarray.NDList
  
  
  
}
