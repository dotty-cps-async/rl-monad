package cps.learning


/**
 * Legacy tensor representation trait without scope management.
 * @deprecated Use ScopedTensorRepresentation instead for explicit scope management.
 */
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


/**
 * Type class for converting domain objects to/from tensors with explicit scope.
 * Context bound [S: TensorScope] ensures S has a TensorScope instance.
 *
 * Unlike the legacy TensorRepresentation, this trait requires an explicit scope
 * parameter for tensor creation, ensuring proper lifecycle management.
 *
 * @tparam A The domain type to convert
 * @tparam S The scope type (e.g., NDManager for DJL)
 */
trait ScopedTensorRepresentation[A, S: TensorScope] {
  /**
   * The tensor type produced by this representation.
   */
  type Tensor

  /**
   * Convert a domain object to a tensor within the given scope.
   * The resulting tensor's lifecycle is managed by the scope.
   */
  def toTensor(a: A, scope: S): Tensor

  /**
   * Convert a tensor back to a domain object.
   * Returns None if the tensor cannot be converted.
   */
  def fromTensor(t: Tensor): Option[A]

  /**
   * Get the shape of the tensor that would be produced for this value.
   */
  def tensorShape(a: A): Seq[Int]
}


/**
 * Extension of ScopedTensorRepresentation for fixed-size, batchable types.
 * Enables efficient batch operations with single memory copy.
 *
 * Use this when:
 * - All instances have the same tensor shape
 * - Batch operations are needed for training
 * - You want to avoid the overhead of converting each element separately
 *
 * @tparam A The domain type to convert
 * @tparam S The scope type (e.g., NDManager for DJL)
 */
trait BatchableTensorRepresentation[A, S: TensorScope] extends ScopedTensorRepresentation[A, S] {
  /**
   * The fixed shape required for batching.
   * All instances must produce tensors of this shape.
   */
  def fixedShape: Seq[Int]

  /**
   * Convert a batch of domain objects to a single batched tensor.
   * More efficient than converting each element separately.
   *
   * @param batch The sequence of domain objects to convert
   * @param scope The scope for tensor creation
   * @return A tensor with batch dimension prepended to fixedShape
   */
  def toBatchedTensor(batch: IndexedSeq[A], scope: S): Tensor

  /**
   * Convert a batched tensor back to a sequence of domain objects.
   */
  def fromBatchedTensor(t: Tensor): IndexedSeq[A]
}


/**
 * Extension methods for cleaner syntax when converting to tensors.
 */
extension [A, S: TensorScope](a: A)(using repr: ScopedTensorRepresentation[A, S])
  /**
   * Convert this value to a tensor within the given scope.
   */
  def toTensor(scope: S): repr.Tensor = repr.toTensor(a, scope)

