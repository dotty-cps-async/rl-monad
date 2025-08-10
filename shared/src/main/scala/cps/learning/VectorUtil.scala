package cps.learning

import scala.util.Random

def takeSample[T](xs: Vector[T], n: Int, random: Random): Vector[T] = {
  if (n <= 0 || xs.isEmpty) {
    Vector.empty[T]
  } else {
    val indices = random.shuffle(xs.indices.toVector).take(n)
    indices.map(xs(_))
  }
}

