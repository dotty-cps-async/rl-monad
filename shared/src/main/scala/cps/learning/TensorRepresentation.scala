package cps.learning


trait TensorRepresentation[A] {

  type Tensor 
  
  def toTensor(a: A): Tensor

  def fromTensor(t: Tensor): Option[A]
  
}


trait IntRepresentation[A] extends TensorRepresentation[A] {

  override type Tensor = Int

  def toTensor(a: A): Int

  def fromTensor(t: Int): Option[A]
  
  def toInt(a:A): Int = {
    toTensor(a)
  }
  
  def fromInt(t: Int): Option[A] = {
    fromTensor(t)
  }

}

