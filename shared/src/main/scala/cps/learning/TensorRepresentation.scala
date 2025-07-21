package cps.learning


trait TensorRepresentation[A] {

  type Tensor
  
  def buildTensor(a: A): Tensor

  def fromTensor(t: Tensor): Option[A]
  
}


trait IntRepresentation[A] extends TensorRepresentation[A] {

  override type Tensor = Int

  def buildTensor(a: A): Int

  def fromTensor(t: Int): Option[A]

}

