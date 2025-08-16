package cps.learning.ds


trait AsPriorityQueue[H[_, _], R] {

  def rOrdering: Ordering[R]
  
  def enqueue[A](a: A, priority: R, queue:H[A,R]): H[A, R]

  def dequeue[A](queue: H[A,R]): (Option[A], H[A, R])

  def peek[A](queue: H[A,R]): Option[A]

  def size[A](queue: H[A,R]): Int
  
}