package cps.learning.backends.jql

import ai.djl.ndarray.NDArray

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ReplayBuffer[S:NDArrayRepresentation,A:NDArrayRepresentation](
  buffer: ArrayBuffer[Experience[S,A]],
  maxSize: Int = 10000,
                       ) {


  def addExperience(experience: Experience[S,A]): Unit = {
    if (buffer.size >= maxSize) {
      buffer.remove(0)
    }
    buffer.append(experience)
  }

  def sample(batchSize: Int, random: Random): Array[Experience[S,A]] = {
    val indices = random.shuffle(buffer.indices.toList).take(batchSize)
    indices.map(buffer(_)).toArray
  }
  
}

object ReplayBuffer {
  
  val DEFAULT_MAX_SIZE = 10000
  
}