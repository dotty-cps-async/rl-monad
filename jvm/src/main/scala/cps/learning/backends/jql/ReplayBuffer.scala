package cps.learning.backends.jql

import cps.learning.IntRepresentation

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ReplayBuffer[O, A: IntRepresentation](
  buffer: ArrayBuffer[Experience[O, A]],
  maxSize: Int = 10000,
                       ) {


  def addExperience(experience: Experience[O, A]): Unit = {
    if (buffer.size >= maxSize) {
      buffer.remove(0)
    }
    buffer.append(experience)
  }

  def sample(batchSize: Int, random: Random): Array[Experience[O, A]] = {
    val indices = random.shuffle(buffer.indices.toList).take(batchSize)
    indices.map(buffer(_)).toArray
  }

}

object ReplayBuffer {
  
  val DEFAULT_MAX_SIZE = 10000
  
}