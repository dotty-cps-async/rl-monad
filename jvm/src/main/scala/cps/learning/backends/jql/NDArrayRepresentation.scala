package cps.learning.backends.jql

import ai.djl.ndarray.{NDArray, NDList}
import ai.djl.translate.{Translator, TranslatorContext}
import cps.learning.TensorRepresentation

trait NDArrayRepresentation[A] extends TensorRepresentation[A] {

  type Tensor = NDArray

  def toNDArray(a: A): NDArray

  def fromNDArray(a: NDArray): Option[A]

  @SuppressWarnings(Array("PMD.SignatureDeclareThrowsException"))
  @throws[Exception]
  override def processInput(ctx: TranslatorContext, input: A): NDList =
    new NDList(toNDArray(input))

  override def processOutput(ctx: TranslatorContext, list: NDList): A = {  
    val ndArray = list.singletonOrThrow()
    fromNDArray(ndArray).getOrElse(throw new IllegalArgumentException("Invalid NDArray"))
  }
  
}

