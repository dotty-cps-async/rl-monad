package cps.rl


trait TensorElementType[R] {

  def toFloat(r: R): Float
  def toDouble(r: R): Double
  def toInt(r: R): Int
  
}

object TensorElementType {
  
  given TensorElementType[Int] with {
    def toFloat(r: Int): Float = r.toFloat
    def toDouble(r: Int): Double = r.toDouble
    def toInt(r: Int): Int = r
  }

  given TensorElementType[Float] with {
    def toFloat(r: Float): Float = r
    def toDouble(r: Float): Double = r.toDouble
    def toInt(r: Float): Int = r.toInt
  }

  given TensorElementType[Double] with {
    def toFloat(r: Double): Float = r.toFloat
    def toDouble(r: Double): Double = r
    def toInt(r: Double): Int = r.toInt
  }
  
}


trait TensorType[T]  {
  
  def shape(t:T): Seq[Int]
  
  def isEmpty(t:T): Boolean
  
  def size(t:T): Int
  
  def reshape(t:T, newShape: Seq[Int]): T
  
  def slice(t:T, start: Seq[Int], end: Seq[Int]): T
  
  def get[R:TensorElementType](t:T, indices: Seq[Int]): R
  
}
