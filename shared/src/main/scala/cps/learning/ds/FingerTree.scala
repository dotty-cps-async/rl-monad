package cps.learning.ds


/**
 * Fingdr tree, used as a priiority queue (which is used as CpsScoredMonad).
 * See
 *
 * HINZE, RALF, and ROSS PATERSON.
 * “Finger Trees: A Simple General-Purpose Data Structure.”
 * Journal of Functional Programming 16, no. 2 (2006): 197–217.
 * https://doi.org/10.1017/S0956796805005769.
 *
 * Below is a minimal implementation of finger tree, used as a basic for priority queue implementation.
 */
sealed trait FingerTree[A, R] {

  def rMonoid: FingerTree.Monoid[R]

  def rMeasured: Measured[A, R]

  def measure: R

  def isEmpty: Boolean

  def prepended(a: A): FingerTree[A, R]

  def +:(a: A): FingerTree[A, R] = prepended(a)

  def prependedAll(as: Iterable[A]): FingerTree[A, R] =
    as.foldRight(this)((a, acc) => acc.prepended(a))

  def ++:(as: Iterable[A]): FingerTree[A, R] =
    prependedAll(as)

  def prependedDigit(d: FingerTree.Digit[A]): FingerTree[A, R] = d match {
    case FingerTree.Digit.One(a) => prepended(a)
    case FingerTree.Digit.Two(a, b) => prepended(b).prepended(a)
    case FingerTree.Digit.Three(a, b, c) => prepended(c).prepended(b).prepended(a)
    case FingerTree.Digit.Four(a, b, c, d) => prepended(d).prepended(c).prepended(b).prepended(a)
  }

  def ++:(d: FingerTree.Digit[A]): FingerTree[A, R] = prependedDigit(d)

  def appended(a: A): FingerTree[A, R]

  def :+(a: A): FingerTree[A, R] = appended(a)

  def appendedAll(as: Iterable[A]): FingerTree[A, R] =
    as.foldLeft(this)((acc, a) => acc.appended(a))

  def :++(as: Iterable[A]): FingerTree[A, R] = appendedAll(as)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
    case FingerTree.Empty() => z
    case FingerTree.Single(a) => op(z, a)
    case FingerTree.Deep(prefix, middle, suffix, minScore) =>
      val zPrefix = prefix.foldLeft(z)(op)
      val zMiddle = middle.foldLeft(zPrefix)((acc, node) => node.foldLeft(acc)(op))
      suffix.foldLeft(zMiddle)(op)
  }

  def foldRight[B](z: B)(op: (A, B) => B): B = this match {
    case FingerTree.Empty() => z
    case FingerTree.Single(a) => op(a, z)
    case FingerTree.Deep(prefix, middle, suffix, cached) =>
      val zSuffix = suffix.foldRight(z)(op)
      val zMiddle = middle.foldRight(zSuffix)((node, acc) => node.foldRight(acc)(op))
      prefix.foldRight(zMiddle)(op)
  }

  def ++(other: FingerTree[A, R]): FingerTree[A, R] = {
    FingerTree.concat(this, other)(using rMeasured, rMonoid)
  }

  def leftSeqView: FingerTree.SeqView[A, R] = {
    FingerTree.seqViewLeft(this)
  }

  def headOption: Option[A] = leftSeqView match {
    case FingerTree.SeqView.Nil() => None
    case FingerTree.SeqView.Cons(head, _) => Some(head)
  }

  def head: A = leftSeqView match {
    case FingerTree.SeqView.Nil() => throw new NoSuchElementException("head of empty FingerTree")
    case FingerTree.SeqView.Cons(head, _) => head
  }

  def tail: FingerTree[A, R] = leftSeqView match {
    case FingerTree.SeqView.Nil() => throw new NoSuchElementException("tail of empty FingerTree")
    case FingerTree.SeqView.Cons(_, tail) => tail
  }

  def rightSeqView: FingerTree.SeqView[A, R] = {
    FingerTree.seqViewRight(this)
  }

  def last: A = rightSeqView match {
    case FingerTree.SeqView.Nil() => throw new NoSuchElementException("last of empty FingerTree")
    case FingerTree.SeqView.Cons(last, _) => last
  }

  def lastOption: Option[A] = rightSeqView match {
    case FingerTree.SeqView.Nil() => None
    case FingerTree.SeqView.Cons(last, _) => Some(last)
  }

  def split(p: R => Boolean)(acc: R = rMonoid.zero): FingerTree.Split[[X] =>> FingerTree[X, R], A] = {
    FingerTree.splitTree(p, acc, this)(using rMeasured, rMonoid)
  }

  def findMax(using rOrdering: Ordering[R]): Option[A] = {
    if (isEmpty) None
    else {
      FingerTree.splitTree(r => rOrdering.gteq(r, this.measure), rMonoid.zero, this)(using rMeasured, rMonoid) match {
        case FingerTree.Split(_, pivot, _) => Some(pivot)
      }
    }
  }

  def findMaxMeasure(using rOrdering: Ordering[R]): Option[R] = {
    if (isEmpty) None
    else {
      Some(this.measure)
    }
  }

  def dequeueMax(using rOrdering: Ordering[R]): (A, FingerTree[A, R]) = {

    def p(r: R): Boolean = {
      rOrdering.gteq(r, this.measure)
    }

    FingerTree.splitTree(p, rMonoid.zero, this)(using rMeasured, rMonoid) match {
      case FingerTree.Split(left, pivot, right) =>
        (pivot, FingerTree.concat(left, right)(using rMeasured, rMonoid))
    }
  }

}

object FingerTree {


  trait Monoid[R] {
    def zero: R

    def combine(x: R, y: R): R
  }


  extension [R](self: R)(using m: Monoid[R]) {
    def |+|(other: R): R = m.combine(self, other)
  }

  def combineMeasures[A, R](measures: A*)(using m: Measured[A, R], r: Monoid[R]): R = {
    measures.foldLeft(summon[Monoid[R]].zero) { (acc, a) =>
      r.combine(acc, summon[Measured[A, R]].measure(a))
    }
  }

  def empty[A, R](using Monoid[R], Measured[A, R]): FingerTree[A, R] = {
    FingerTree.Empty()
  }

  case class Empty[A, R]()(using Monoid[R], Measured[A, R]) extends FingerTree[A, R] {

    def rMonoid: Monoid[R] = summon[Monoid[R]]

    def rMeasured: Measured[A, R] = summon[Measured[A, R]]

    def measure: R = rMonoid.zero

    def isEmpty: Boolean = true

    def prepended(a: A): FingerTree[A, R] = Single(a)

    def appended(a: A): FingerTree[A, R] = Single(a)

  }

  case class Single[A, R](a: A)(using Monoid[R], Measured[A, R]) extends FingerTree[A, R] {
    def rMonoid: Monoid[R] = summon[Monoid[R]]

    def rMeasured: Measured[A, R] = summon[Measured[A, R]]

    def measure: R = summon[Measured[A, R]].measure(a)

    def isEmpty: Boolean = false

    def prepended(a: A): FingerTree[A, R] = {
      val newMeasure = rMonoid.combine(measure, summon[Measured[A, R]].measure(a))
      Deep(Digit.One(a), FingerTree.Empty(), Digit.One(this.a), newMeasure)
    }

    def appended(a: A): FingerTree[A, R] = {
      val newMeasure = measure |+| summon[Measured[A, R]].measure(a)
      Deep(Digit.One(this.a), FingerTree.Empty(), Digit.One(a), newMeasure)
    }

  }

  case class Deep[A, R](prefix: Digit[A], middle: FingerTree[Node[A, R], R], suffix: Digit[A], measure: R)(using Monoid[R], Measured[A, R]) extends FingerTree[A, R] {

    def rMonoid: Monoid[R] = summon[Monoid[R]]

    override def rMeasured: Measured[A, R] = summon[Measured[A, R]]

    def isEmpty: Boolean = false

    def prepended(a: A): FingerTree[A, R] = {
      val newMeasure = rMonoid.combine(measure, summon[Measured[A, R]].measure(a))
      prefix match
        case Digit.One(b) => Deep(Digit.Two(a, b), middle, suffix, newMeasure)
        case Digit.Two(b, c) => Deep(Digit.Three(a, b, c), middle, suffix, newMeasure)
        case Digit.Three(b, c, d) => Deep(Digit.Four(a, b, c, d), middle, suffix, newMeasure)
        case Digit.Four(b, c, d, e) =>
          val newPrefix = Digit.Two(a, b)
          val node = Node.Node3(c, d, e, combineMeasures(c, d, e))
          Deep(newPrefix, middle.prepended(node), suffix, newMeasure)
    }

    def appended(a: A): FingerTree[A, R] = {
      val newMeasure = rMonoid.combine(measure, summon[Measured[A, R]].measure(a))
      suffix match
        case Digit.One(b) => Deep(prefix, middle, Digit.Two(b, a), newMeasure)
        case Digit.Two(b, c) => Deep(prefix, middle, Digit.Three(b, c, a), newMeasure)
        case Digit.Three(b, c, d) => Deep(prefix, middle, Digit.Four(b, c, d, a), newMeasure)
        case Digit.Four(b, c, d, e) =>
          val newSuffix = Digit.Two(e, a)
          val node = Node.Node3(b, c, d, combineMeasures(b, c, d))
          Deep(prefix, middle.appended(node), newSuffix, newMeasure)
    }

  }


  enum Node[A, R] {

    def measure: R

    case Node2(a: A, b: A, override val measure: R) extends Node[A, R]
    case Node3(a: A, b: A, c: A, override val measure: R) extends Node[A, R]

    def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
      case Node2(a, b, m) => op(op(z, a), b)
      case Node3(a, b, c, m) => op(op(op(z, a), b), c)
    }

    def foldRight[B](z: B)(op: (A, B) => B): B = this match {
      case Node2(a, b, m) => op(a, op(b, z))
      case Node3(a, b, c, m) => op(a, op(b, op(c, z)))
    }

    def toDigit: Digit[A] = this match {
      case Node2(a, b, _) => Digit.Two(a, b)
      case Node3(a, b, c, _) => Digit.Three(a, b, c)
    }

    def find(p: R => Boolean, acc: R)(using m: Measured[A, R], r: Monoid[R]): Option[A] = this match {
      case Node2(a, b, measure) =>
        if (p(acc |+| m.measure(a))) then Some(a)
        else if (p(acc |+| m.measure(b))) then Some(b)
        else None
      case Node3(a, b, c, measure) =>
        if (p(acc |+| m.measure(a)) == a) then Some(a)
        else if (p(acc |+| m.measure(b)) == b) then Some(b)
        else if (p(acc |+| m.measure(c)) == c) then Some(c)
        else None
    }

  }

  given [A, R](using m: Measured[A, R], r: Monoid[R]): Measured[FingerTree[A, R], R] with {
    def measure(tree: FingerTree[A, R]): R = tree match {
      case Empty() => r.zero
      case Single(a) => m.measure(a)
      case Deep(prefix, middle, suffix, measure) => measure
    }
  }

  given [A, R](using m: Measured[A, R], r: Monoid[R]): Measured[Node[A, R], R] with {
    def measure(node: Node[A, R]): R = node.measure
  }

  enum Digit[A] {
    case One(a: A) extends Digit[A]
    case Two(a: A, b: A) extends Digit[A]
    case Three(a: A, b: A, c: A) extends Digit[A]
    case Four(a: A, b: A, c: A, d: A) extends Digit[A]

    def foldLeft[B](z: B)(op: (B, A) => B): B = this match {
      case One(a) => op(z, a)
      case Two(a, b) => op(op(z, a), b)
      case Three(a, b, c) => op(op(op(z, a), b), c)
      case Four(a, b, c, d) => op(op(op(op(z, a), b), c), d)
    }

    def foldRight[B](z: B)(op: (A, B) => B): B = this match {
      case One(a) => op(a, z)
      case Two(a, b) => op(a, op(b, z))
      case Three(a, b, c) => op(a, op(b, op(c, z)))
      case Four(a, b, c, d) => op(a, op(b, op(c, op(d, z))))
    }

    def measure[R](implicit m: Measured[A, R], r: Monoid[R]): R = this match {
      case One(a) => m.measure(a)
      case Two(a, b) => r.combine(m.measure(a), m.measure(b))
      case Three(a, b, c) => r.combine(r.combine(m.measure(a), m.measure(b)), m.measure(c))
      case Four(a, b, c, d) => r.combine(r.combine(r.combine(m.measure(a), m.measure(b)), m.measure(c)), m.measure(d))
    }

    def head: A = this match {
      case One(a) => a
      case Two(a, _) => a
      case Three(a, _, _) => a
      case Four(a, _, _, _) => a
    }

    def tailOption: Option[Digit[A]] = this match {
      case One(_) => None
      case Two(_, b) => Some(One(b))
      case Three(_, b, c) => Some(Two(b, c))
      case Four(_, b, c, d) => Some(Three(b, c, d))
    }

    def toList: List[A] = this match {
      case One(a) => List(a)
      case Two(a, b) => List(a, b)
      case Three(a, b, c) => List(a, b, c)
      case Four(a, b, c, d) => List(a, b, c, d)
    }

    def toLazyList: LazyList[A] = this match {
      case One(a) => LazyList(a)
      case Two(a, b) => LazyList(a, b)
      case Three(a, b, c) => LazyList(a, b, c)
      case Four(a, b, c, d) => LazyList(a, b, c, d)
    }

    def toTree[R](using m: Measured[A, R], r: Monoid[R]): FingerTree[A, R] = this match {
      case One(a) => FingerTree.Single(a)
      case Two(a, b) => FingerTree.Deep(Digit.One(a), FingerTree.Empty(), Digit.One(b), m.measure(a) |+| m.measure(b))
      case Three(a, b, c) => FingerTree.Deep(Digit.Two(a, b), FingerTree.Empty(), Digit.One(c), m.measure(a) |+| m.measure(b) |+| m.measure(c))
      case Four(a, b, c, d) => FingerTree.Deep(Digit.Two(a, b), FingerTree.Empty(), Digit.Two(c, d), m.measure(a) |+| m.measure(b) |+| m.measure(c) |+| m.measure(d))
    }

    def last: A = this match {
      case One(a) => a
      case Two(_, b) => b
      case Three(_, b, c) => c
      case Four(_, b, c, d) => d
    }

    def removedLast: Option[Digit[A]] = this match {
      case One(_) => None
      case Two(a, _) => Some(One(a))
      case Three(a, b, _) => Some(Two(a, b))
      case Four(a, b, c, _) => Some(Three(a, b, c))
    }

    def find[R](p: R => Boolean, acc: R)(using m: Measured[A, R], r: Monoid[R]): Option[A] = {
      this match
        case One(a) =>
          if p(acc |+| m.measure(a)) then Some(a) else None
        case Two(a, b) =>
          val acc1 = acc |+| m.measure(a)
          if p(acc1) then Some(a)
          else {
            val acc2 = acc1 |+| m.measure(b)
            if p(acc2) then Some(b) else None
          }
        case Three(a, b, c) =>
          val acc1 = acc |+| m.measure(a)
          if p(acc1) then Some(a)
          else {
            val acc2 = acc1 |+| m.measure(b)
            if (p(acc2) == b) Some(b)
            else {
              val acc3 = acc2 |+| m.measure(c)
              if (p(acc3) == c) Some(c) else None
            }
          }
        case Four(a, b, c, d) =>
          val acc1 = acc |+| m.measure(a)
          if (p(acc1) == a) Some(a)
          else {
            val acc2 = acc1 |+| m.measure(b)
            if (p(acc2) == b) Some(b)
            else {
              val acc3 = acc2 |+| m.measure(c)
              if (p(acc3) == c) Some(c)
              else {
                val acc4 = acc3 |+| m.measure(d)
                if (p(acc4) == d) Some(d) else None
              }
            }
          }
    }

  }

  extension [A](optD: Option[Digit[A]]) {
    def toTree[R](using Measured[A, R], Monoid[R]): FingerTree[A, R] = optD match {
      case None => FingerTree.Empty()
      case Some(d) => d.toTree
    }
  }

  enum SeqView[A, R] {
    case Nil[A, R]() extends SeqView[A, R]
    case Cons[A, R](head: A, tail: FingerTree[A, R]) extends SeqView[A, R]
  }

  def concat[A, R](left: FingerTree[A, R], right: FingerTree[A, R])(using Measured[A, R], Monoid[R]): FingerTree[A, R] = {

    def nodes[A, R](l: List[A])(using Measured[A, R], Monoid[R]): List[Node[A, R]] = {
      // note that size of list is always bigger than 1, because digits from leaft and right are always at least 1 element
      l match
        case Seq(a, b) => List(Node.Node2(a, b, combineMeasures(a, b)))
        case Seq(a, b, c) => List(Node.Node3(a, b, c, combineMeasures(a, b, c)))
        case Seq(a, b, c, d) => List(Node.Node2(a, b, combineMeasures(a, b)), Node.Node2(c, d, combineMeasures(c, d)))
        case Seq(a, b, c, rest@_*) => Node.Node3(a, b, a, combineMeasures(a, b, c)) +: nodes(rest.toList)
        case _ => throw new IllegalArgumentException("Invalid list for nodes creation")
    }

    def app3[A, R](l: FingerTree[A, R], m: List[A], r: FingerTree[A, R])(using Measured[A, R], Monoid[R]): FingerTree[A, R] = {
      l match
        case FingerTree.Empty() =>
          m ++: r
        case FingerTree.Single(a) =>
          a +: (m ++: r)
        case deepL: FingerTree.Deep[A, R] =>
          r match
            case FingerTree.Empty() =>
              l :++ m
            case FingerTree.Single(b) =>
              l :++ (m :+ b)
            case deepR: FingerTree.Deep[A, R] =>
              val newMiddle = app3(deepL.middle, nodes(deepL.suffix.toList ++ m ++ deepR.prefix.toList), deepR.middle)
              Deep(
                deepL.prefix,
                newMiddle,
                deepR.suffix,
                deepL.prefix.measure |+| newMiddle.measure |+| deepR.suffix.measure
              )
    }

    app3(left, List.empty, right)

  }

  def seqViewLeft[A, R](tree: FingerTree[A, R]): SeqView[A, R] = {
    given Measured[A, R] = tree.rMeasured

    given Monoid[R] = tree.rMonoid

    tree match {
      case FingerTree.Empty() => SeqView.Nil()
      case FingerTree.Single(a) => SeqView.Cons(a, FingerTree.Empty())
      case FingerTree.Deep(prefix, middle, suffix, measure) =>
        val frs = prefix.head
        val rest = deepL(prefix.tailOption, middle, suffix)
        SeqView.Cons(frs, rest)
    }
  }

  def seqViewRight[A, R](tree: FingerTree[A, R]): SeqView[A, R] = {
    given Measured[A, R] = tree.rMeasured

    given Monoid[R] = tree.rMonoid

    tree match {
      case FingerTree.Empty() => SeqView.Nil()
      case FingerTree.Single(a) => SeqView.Cons(a, FingerTree.Empty())
      case FingerTree.Deep(prefix, middle, suffix, measure) =>
        val lfs = suffix.last
        val rest = deepR(prefix, middle, suffix.removedLast)
        SeqView.Cons(lfs, rest)
    }
  }

  def deepL[A, R](optLeft: Option[Digit[A]], middle: FingerTree[Node[A, R], R], right: Digit[A])(using Measured[A, R], Monoid[R]): FingerTree[A, R] = {
    optLeft match {
      case None =>
        seqViewLeft(middle) match
          case SeqView.Nil() => right.toTree
          case SeqView.Cons(head, tail) =>
            FingerTree.Deep(head.toDigit, tail, right, head.measure |+| tail.measure |+| right.measure)
      case Some(left) =>
        FingerTree.Deep(left, middle, right, left.measure |+| middle.measure |+| right.measure)
    }
  }

  def deepR[A, R](left: Digit[A], middle: FingerTree[Node[A, R], R], optRight: Option[Digit[A]])(using Measured[A, R], Monoid[R]): FingerTree[A, R] = {
    optRight match {
      case None =>
        seqViewRight(middle) match
          case SeqView.Nil() => left.toTree
          case SeqView.Cons(head, tail) =>
            FingerTree.Deep(left, tail, head.toDigit, left.measure |+| tail.measure |+| head.measure)
      case Some(right) =>
        FingerTree.Deep(left, middle, right, left.measure |+| middle.measure |+| right.measure)
    }
  }

  case class Split[F[_], A](left: F[A], pivot: A, right: F[A])

  def splitList[A, R](l: List[A], acc: R, p: R => Boolean)(using Measured[A, R], Monoid[R]): Split[List, A] = {
    l match {
      case Nil => throw new IllegalArgumentException("Cannot split an empty list")
      case head :: tail =>
        val acc1 = acc |+| summon[Measured[A, R]].measure(head)
        if (p(acc1)) Split(Nil, head, tail)
        else {
          val splitTail = splitList(tail, acc1, p)
          Split(head :: splitTail.left, splitTail.pivot, splitTail.right)
        }
    }
  }

  def splitDigit[A, R](d: Digit[A], acc: R, p: R => Boolean)(using Measured[A, R], Monoid[R]): Split[[X] =>> Option[Digit[X]], A] = {
    d match {
      case Digit.One(a) =>
        Split(None, a, None)
      case Digit.Two(a, b) =>
        val acc1 = acc |+| summon[Measured[A, R]].measure(a)
        if (p(acc1)) Split(None, a, Some(Digit.One(b)))
        else {
          val lxrSplit = splitDigit(Digit.One(b), acc1, p)
          lxrSplit.left match {
            case None => Split(Some(Digit.One(a)), lxrSplit.pivot, lxrSplit.right)
            case Some(d) =>
              throw new IllegalArgumentException("Unexpected left digit in splitDigit(One) ")
          }
        }
      case Digit.Three(a, b, c) =>
        val acc1 = acc |+| summon[Measured[A, R]].measure(a)
        if (p(acc1)) then Split(None, a, Some(Digit.Two(b, c)))
        else
          val lxrSplit = splitDigit(Digit.Two(b, c), acc1, p)
          lxrSplit.left match
            case None => Split(Some(Digit.One(a)), lxrSplit.pivot, lxrSplit.right)
            case Some(Digit.One(la)) =>
              Split(Some(Digit.Two(a, la)), lxrSplit.pivot, lxrSplit.right)
            case Some(_) =>
              throw new IllegalArgumentException("Unexpected left digit in splitDigit(Two) ")
      case Digit.Four(a, b, c, d) =>
        val acc1 = acc |+| summon[Measured[A, R]].measure(a)
        if (p(acc1)) Split(None, a, Some(Digit.Three(b, c, d)))
        else
          val lxrSplit = splitDigit(Digit.Three(b, c, d), acc1, p)
          lxrSplit.left match {
            case None => Split(Some(Digit.One(a)), lxrSplit.pivot, lxrSplit.right)
            case Some(Digit.One(x)) =>
              Split(Some(Digit.Two(a, x)), lxrSplit.pivot, lxrSplit.right)
            case Some(Digit.Two(x, y)) =>
              Split(Some(Digit.Three(a, x, y)), lxrSplit.pivot, lxrSplit.right)
            case Some(_) =>
              throw new IllegalArgumentException("Unexpected left digit in splitDigit(Three) ")
          }
    }
  }

  def splitTree[A, R](p: R => Boolean, acc: R, tree: FingerTree[A, R])(using Measured[A, R], Monoid[R]): Split[[X] =>> FingerTree[X, R], A] = {
    tree match
      case FingerTree.Empty() => throw new IllegalArgumentException("Cannot split an empty tree")
      case FingerTree.Single(a) =>
        Split(FingerTree.Empty(), a, FingerTree.Empty())
      case FingerTree.Deep(prefix, middle, suffix, measure) =>
        val vpr = acc |+| prefix.measure
        if (p(vpr)) then
          val lxrSplit = splitDigit(prefix, acc, p)
          Split(lxrSplit.left.toTree, lxrSplit.pivot, deepL(lxrSplit.right, middle, suffix))
        else
          val vm = vpr |+| middle.measure
          if p(vm) then
            val middleSplit = splitTree(p, vpr, middle)
            val lxrSplit = splitDigit(middleSplit.pivot.toDigit, vpr |+| middleSplit.left.measure, p)
            Split(deepR(prefix, middleSplit.left, lxrSplit.left),
              lxrSplit.pivot,
              deepL(lxrSplit.right, middleSplit.right, suffix))
          else
            val lxrSplit = splitDigit(suffix, vm, p)
            Split(deepR(prefix, middle, lxrSplit.left),
              lxrSplit.pivot,
              lxrSplit.right match {
                case None => FingerTree.Empty()
                case Some(d) => d.toTree
              }
            )

  }

  /**
   * Find an element in the tree that satisfies the predicate `p` when combined with the accumulated measure `acc`.
   * Returns the first element that satisfies the condition.
   */
  def find[A, R](p: R => Boolean, acc: R, tree: FingerTree[A, R])(using Measured[A, R], Monoid[R], Ordering[R]): Option[A] = {
    tree match {
      case FingerTree.Empty() => None
      case FingerTree.Single(a) =>
        if (p(acc |+| summon[Measured[A, R]].measure(a))) Some(a)
        else None
      case FingerTree.Deep(prefix, middle, suffix, measure) =>
        prefix.find(p, acc).orElse {
          val acc1 = acc |+| prefix.measure
          find(p, acc1, middle) match
            case Some(node) => node.find(p, acc1)
            case None =>
              suffix.find(p, acc1 |+| middle.measure)
        }
    }
  }


}
