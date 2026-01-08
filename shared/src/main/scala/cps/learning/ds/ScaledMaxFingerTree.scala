package cps.learning.ds

import cps.learning.*

/**
 * Scaled Finger tree, used as a priority queue with scaling factors.
 * Based on the finger tree implementation but with scaled factor in each tree
 * and fixed monoid (max operation) over ordering.
 */
sealed trait ScaledMaxFingerTree[A: Measured.Curry1[R], R: ScalingGroup : Ordering] {

  def measured: Measured[A, R] = summon[Measured[A, R]]

  def rGroup: ScalingGroup[R] = summon[ScalingGroup[R]]

  def rOrdering: Ordering[R] = summon[Ordering[R]]

  def measure: R

  def isEmpty: Boolean

  def prepended(a: A): ScaledMaxFingerTree[A, R] = prepended(ScaledValue(a, rGroup.one))

  def prepended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R]

  def prependedAll(as: Iterable[ScaledValue[A, R]]): ScaledMaxFingerTree[A, R] =
    as.foldRight(this)((a, acc) => acc.prepended(a))

  def ++:(as: Iterable[ScaledValue[A, R]]): ScaledMaxFingerTree[A, R] =
    prependedAll(as)


  def +:(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] = prepended(a)

  def appended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R]

  def appended(a: A): ScaledMaxFingerTree[A, R] = appended(ScaledValue(a, rGroup.one))

  def appendedAll(as: Iterable[ScaledValue[A, R]]): ScaledMaxFingerTree[A, R] =
    as.foldLeft(this)((acc, a) => acc.appended(a))

  def :++(as: Iterable[ScaledValue[A, R]]): ScaledMaxFingerTree[A, R] =
    appendedAll(as)


  def :+(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] = appended(a)

  def scale(factor: R): ScaledMaxFingerTree[A, R]

  def splitMax(): ScaledMaxFingerTree.Split[[X] =>> ScaledMaxFingerTree[X, R], A] = {
    ScaledMaxFingerTree.splitMax(this).toSplit
  }

  def dequeueMax: (Option[A], ScaledMaxFingerTree[A, R]) = {
    if (isEmpty) then
      (None, this)
    else
      val split = splitMax()
      (Some(split.pivot), ScaledMaxFingerTree.concat(split.left, split.right))
  }

  def findMax: Option[A] = {
    if (isEmpty) then None
    else
      val split = splitMax()
      Some(split.pivot)
  }

}


object ScaledMaxFingerTree {

  // Helper for max using Ordering
  private def maxOf[R: Ordering](values: R*): R = values.max

  given scaledMeasured[A, R](using Measured[A, R], ScalingGroup[R]): Measured[ScaledValue[A, R], R] with {
    def measure(value: ScaledValue[A, R]): R =
      summon[ScalingGroup[R]].scaleBy(summon[Measured[A, R]].measure(value.value), value.factor)
  }

  given [A, R](using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): Measured[ScaledMaxFingerTree[A, R], R] with {
    def measure(tree: ScaledMaxFingerTree[A, R]): R = tree match {
      case Empty() => rG.zero
      case Single(value) => rG.scaleBy(m.measure(value.value), value.factor)
      case Deep(left, middle, right) =>
        val leftMeasure = left.measure
        val middleMeasure = middle.measure
        val rightMeasure = right.measure
        ord.max(ord.max(leftMeasure, middleMeasure), rightMeasure)
    }
  }

  given [A, R](using m: Measured[A, R], rGroup: ScalingGroup[R]): Measured[Node[A, R], R] with {
    def measure(node: Node[A, R]): R = node.measure
  }

  extension [A, L](a: A)(using lGroup: ScalingGroup[L]) {
    def scaled(factor: L): ScaledValue[A, L] = ScaledValue(a, factor)
  }

  case class Empty[A: Measured.Curry1[R], R: ScalingGroup : Ordering]() extends ScaledMaxFingerTree[A, R] {

    def measure: R = rGroup.zero

    def isEmpty: Boolean = true

    def prepended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] =
      Single(a)

    def appended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] =
      Single(a)

    def scale(factor: R): ScaledMaxFingerTree[A, R] = this
  }

  case class Single[A: Measured.Curry1[R], R: ScalingGroup : Ordering](value: ScaledValue[A, R]) extends ScaledMaxFingerTree[A, R] {

    def measure: R = summon[Measured[ScaledValue[A, R], R]].measure(value)

    def isEmpty: Boolean = false

    def prepended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] =
      Deep(Digit.One(a), Empty(), Digit.One(value))

    def appended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] =
      Deep(Digit.One(value), Empty(), Digit.One(a))

    def scale(factor: R): ScaledMaxFingerTree[A, R] =
      Single(value.copy(factor = value.factor |*| factor))
  }

  case class Deep[A, R: ScalingGroup : Ordering](
                                               left: Digit[A, R],
                                               middle: ScaledMaxFingerTree[Node[A, R], R],
                                               right: Digit[A, R]
                                             )(using Measured[A, R]) extends ScaledMaxFingerTree[A, R] {

    private def combineMeasures(values: ScaledValue[A, R]*): R = {
      values.map(v => rGroup.scaleBy(measured.measure(v.value), v.factor)).max
    }

    def measure: R = {
      val leftMeasure = left.measure
      val middleMeasure = middle.measure
      val rightMeasure = right.measure
      rOrdering.max(rOrdering.max(leftMeasure, middleMeasure), rightMeasure)
    }

    def isEmpty: Boolean = false

    def prepended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] = {
      left match
        case Digit.Four(b, c, d, e) =>
          val node = Node.Node3(c, d, e, combineMeasures(c, d, e))
          Deep(Digit.Two(a, b), middle.prepended(node), right)
        case _ =>
          Deep(left.prepended(a), middle, right)
    }

    def appended(a: ScaledValue[A, R]): ScaledMaxFingerTree[A, R] = {
      right match {
        case Digit.Four(b, c, d, e) =>
          val node = Node.Node3(b, c, d, combineMeasures(b, c, d))
          Deep(left, middle.appended(node), Digit.Two(e, a))
        case _ =>
          Deep(left, middle, right.appended(a))
      }
    }

    def scale(newFactor: R): ScaledMaxFingerTree[A, R] =
      Deep(left.scale(newFactor), middle.scale(newFactor), right.scale(newFactor))

  }

  sealed trait Digit[A, R] {
    def prepended(a: ScaledValue[A, R]): Digit[A, R]

    def appended(a: ScaledValue[A, R]): Digit[A, R]

    def measure(using Measured[A, R], ScalingGroup[R], Ordering[R]): R

    def toTree(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] = this match {
      case Digit.One(a) => Single(a)
      case Digit.Two(a, b) => Deep(Digit.One(a), Empty(), Digit.One(b))
      case Digit.Three(a, b, c) => Deep(Digit.Two(a, b), Empty(), Digit.One(c))
      case Digit.Four(a, b, c, d) => Deep(Digit.Two(a, b), Empty(), Digit.Two(c, d))
    }

    def head: ScaledValue[A, R] = this match {
      case Digit.One(a) => a
      case Digit.Two(a, _) => a
      case Digit.Three(a, _, _) => a
      case Digit.Four(a, _, _, _) => a
    }

    def tailOption: Option[Digit[A, R]] = this match {
      case Digit.One(_) => None
      case Digit.Two(_, b) => Some(Digit.One(b))
      case Digit.Three(_, b, c) => Some(Digit.Two(b, c))
      case Digit.Four(_, b, c, d) => Some(Digit.Three(b, c, d))
    }

    def last: ScaledValue[A, R] = this match {
      case Digit.One(a) => a
      case Digit.Two(_, b) => b
      case Digit.Three(_, _, c) => c
      case Digit.Four(_, _, _, d) => d
    }

    def removedLast: Option[Digit[A, R]] = this match {
      case Digit.One(_) => None
      case Digit.Two(a, _) => Some(Digit.One(a))
      case Digit.Three(a, b, _) => Some(Digit.Two(a, b))
      case Digit.Four(a, b, c, _) => Some(Digit.Three(a, b, c))
    }

    def toList: List[ScaledValue[A, R]] = this match {
      case Digit.One(a) => List(a)
      case Digit.Two(a, b) => List(a, b)
      case Digit.Three(a, b, c) => List(a, b, c)
      case Digit.Four(a, b, c, d) => List(a, b, c, d)
    }

    def scale(x: R)(using ScalingGroup[R]): Digit[A, R] = this match {
      case Digit.One(a) => Digit.One(a.scale(x))
      case Digit.Two(a, b) => Digit.Two(a.scale(x), b.scale(x))
      case Digit.Three(a, b, c) => Digit.Three(a.scale(x), b.scale(x), c.scale(x))
      case Digit.Four(a, b, c, d) => Digit.Four(a.scale(x), b.scale(x), c.scale(x), d.scale(x))
    }

  }

  object Digit {
    case class One[A, R](a: ScaledValue[A, R]) extends Digit[A, R] {
      def prepended(x: ScaledValue[A, R]): Digit[A, R] = Two(x, a)

      def appended(x: ScaledValue[A, R]): Digit[A, R] = Two(a, x)

      def measure(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): R =
        rG.scaleBy(m.measure(a.value), a.factor)
    }

    case class Two[A, R](a: ScaledValue[A, R], b: ScaledValue[A, R]) extends Digit[A, R] {
      def prepended(x: ScaledValue[A, R]): Digit[A, R] = Three(x, a, b)

      def appended(x: ScaledValue[A, R]): Digit[A, R] = Three(a, b, x)

      def measure(using sm: Measured[A, R], fg: ScalingGroup[R], ord: Ordering[R]): R =
        ord.max(fg.scaleBy(sm.measure(a.value), a.factor), fg.scaleBy(sm.measure(b.value), b.factor))
    }

    case class Three[A, R](a: ScaledValue[A, R], b: ScaledValue[A, R], c: ScaledValue[A, R]) extends Digit[A, R] {
      def prepended(x: ScaledValue[A, R]): Digit[A, R] = Four(x, a, b, c)

      def appended(x: ScaledValue[A, R]): Digit[A, R] = Four(a, b, c, x)

      def measure(using sm: Measured[A, R], fg: ScalingGroup[R], ord: Ordering[R]): R = {
        val ma = fg.scaleBy(sm.measure(a.value), a.factor)
        val mb = fg.scaleBy(sm.measure(b.value), b.factor)
        val mc = fg.scaleBy(sm.measure(c.value), c.factor)
        ord.max(ord.max(ma, mb), mc)
      }
    }

    case class Four[A, R](a: ScaledValue[A, R], b: ScaledValue[A, R], c: ScaledValue[A, R], d: ScaledValue[A, R]) extends Digit[A, R] {
      def prepended(x: ScaledValue[A, R]): Digit[A, R] = throw new IllegalStateException("Cannot prepend to Four digit")

      def appended(x: ScaledValue[A, R]): Digit[A, R] = throw new IllegalStateException("Cannot append to Four digit")

      def measure(using sm: Measured[A, R], fg: ScalingGroup[R], ord: Ordering[R]): R = {
        val ma = fg.scaleBy(sm.measure(a.value), a.factor)
        val mb = fg.scaleBy(sm.measure(b.value), b.factor)
        val mc = fg.scaleBy(sm.measure(c.value), c.factor)
        val md = fg.scaleBy(sm.measure(d.value), d.factor)
        ord.max(ord.max(ma, mb), ord.max(mc, md))
      }
    }
  }

  extension [A, R](optd: Option[Digit[A, R]]) {
    def toTree(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] = optd match {
      case None => Empty()
      case Some(d) => d.toTree
    }
  }

  enum Node[A, R] {

    def measure: R

    case Node2(a: ScaledValue[A, R], b: ScaledValue[A, R], override val measure: R) extends Node[A, R]
    case Node3(a: ScaledValue[A, R], b: ScaledValue[A, R], c: ScaledValue[A, R], override val measure: R) extends Node[A, R]

    def foldLeft[B](z: B)(op: (B, ScaledValue[A, R]) => B): B = this match {
      case Node2(a, b, m) => op(op(z, a), b)
      case Node3(a, b, c, m) => op(op(op(z, a), b), c)
    }

    def foldRight[B](z: B)(op: (ScaledValue[A, R], B) => B): B = this match {
      case Node2(a, b, m) => op(a, op(b, z))
      case Node3(a, b, c, m) => op(a, op(b, op(c, z)))
    }

    def toDigit: Digit[A, R] = this match {
      case Node2(a, b, _) => Digit.Two(a, b)
      case Node3(a, b, c, _) => Digit.Three(a, b, c)
    }

    def scale(factor: R)(using m: Measured[A, R], rG: ScalingGroup[R]): Node[A, R] = this match {
      case Node2(a, b, maxAB) =>
        val a1 = a.scale(factor)
        val b1 = b.scale(factor)
        Node2(a1, b1, rG.scaleBy(maxAB, factor))
      case Node3(a, b, c, maxABC) =>
        val a1 = a.scale(factor)
        val b1 = b.scale(factor)
        val c1 = c.scale(factor)
        Node3(a1, b1, c1, rG.scaleBy(maxABC, factor))
    }

  }

  extension [A, R](self: ScaledValue[Node[A, R], R]) {
    def toDigit(using Measured[A, R], ScalingGroup[R]): Digit[A, R] = self.value.scale(self.factor).toDigit
  }

  def empty[A, R](using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] = Empty()

  def singleton[A, R](value: A)(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] =
    Single(ScaledValue(value, rG.one))

  case class Split[F[_], A](left: F[A], pivot: A, right: F[A])

  case class ScaledSplit[F[_], A, R](left: F[A], pivot: ScaledValue[A, R], right: F[A]) {
    def toSplit: Split[F, A] = Split(left, pivot.value, right)
  }

  enum SeqView[A, R] {
    case Nil[A, R]() extends SeqView[A, R]
    case Cons[A, R](head: ScaledValue[A, R], tail: ScaledMaxFingerTree[A, R]) extends SeqView[A, R]
  }

  def seqViewLeft[A, R](tree: ScaledMaxFingerTree[A, R])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): SeqView[A, R] = {
    tree match {
      case Empty() => SeqView.Nil()
      case Single(value) => SeqView.Cons(value, Empty())
      case Deep(prefix, middle, suffix) =>
        val head = prefix.head
        val rest = deepL(prefix.tailOption, middle, suffix)
        SeqView.Cons(head, rest)
    }
  }

  def seqViewRight[A, R](tree: ScaledMaxFingerTree[A, R])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): SeqView[A, R] = {
    tree match {
      case Empty() => SeqView.Nil()
      case Single(value) => SeqView.Cons(value, Empty())
      case Deep(prefix, middle, suffix) =>
        val head = suffix.last
        val rest = deepR(prefix, middle, suffix.removedLast)
        SeqView.Cons(head, rest)
    }
  }

  def deepL[A, R](optLeft: Option[Digit[A, R]], middle: ScaledMaxFingerTree[Node[A, R], R], right: Digit[A, R])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] = {
    optLeft match {
      case None =>
        seqViewLeft(middle) match
          case SeqView.Nil() => right.toTree
          case SeqView.Cons(head, tail) =>
            val node = head.value.scale(head.factor)
            Deep(node.toDigit, tail, right)
      case Some(left) =>
        Deep(left, middle, right)
    }
  }

  def deepR[A, R](left: Digit[A, R], middle: ScaledMaxFingerTree[Node[A, R], R], optRight: Option[Digit[A, R]])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] = {
    optRight match {
      case None =>
        seqViewRight(middle) match
          case SeqView.Nil() => left.toTree
          case SeqView.Cons(head, tail) =>
            val node = head.value.scale(head.factor)
            Deep(left, tail, node.toDigit)
      case Some(right) =>
        Deep(left, middle, right)
    }
  }

  def splitMaxDigit[A, R](d: Digit[A, R])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledSplit[[X] =>> Option[Digit[X, R]], A, R] = {
    d match {
      case Digit.One(a) =>
        ScaledSplit(None, a, None)
      case Digit.Two(a, b) =>
        val ma = rG.scaleBy(m.measure(a.value), a.factor)
        val mb = rG.scaleBy(m.measure(b.value), b.factor)
        //TODO: use instead gtEx something like approximate equality with epsilon
        if (ord.gteq(ma, mb)) ScaledSplit(None, a, Some(Digit.One(b)))
        else ScaledSplit(Some(Digit.One(a)), b, None)
      case Digit.Three(a, b, c) =>
        val ma = rG.scaleBy(m.measure(a.value), a.factor)
        val mb = rG.scaleBy(m.measure(b.value), b.factor)
        val mc = rG.scaleBy(m.measure(c.value), c.factor)
        val maxMeasure = ord.max(ord.max(ma, mb), mc)
        //TODO: use equalsApprox
        if (ord.equiv(ma, maxMeasure)) ScaledSplit(None, a, Some(Digit.Two(b, c)))
        else if (ord.equiv(mb, maxMeasure)) ScaledSplit(Some(Digit.One(a)), b, Some(Digit.One(c)))
        else ScaledSplit(Some(Digit.Two(a, b)), c, None)
      case Digit.Four(a, b, c, d) =>
        val ma = rG.scaleBy(m.measure(a.value), a.factor)
        val mb = rG.scaleBy(m.measure(b.value), b.factor)
        val mc = rG.scaleBy(m.measure(c.value), c.factor)
        val md = rG.scaleBy(m.measure(d.value), d.factor)
        val maxMeasure = ord.max(ord.max(ma, mb), ord.max(mc, md))
        if (ord.equiv(ma, maxMeasure)) ScaledSplit(None, a, Some(Digit.Three(b, c, d)))
        else if (ord.equiv(mb, maxMeasure)) ScaledSplit(Some(Digit.One(a)), b, Some(Digit.Two(c, d)))
        else if (ord.equiv(mc, maxMeasure)) ScaledSplit(Some(Digit.Two(a, b)), c, Some(Digit.One(d)))
        else ScaledSplit(Some(Digit.Three(a, b, c)), d, None)
    }
  }


  def splitMax[A, R](tree: ScaledMaxFingerTree[A, R])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledSplit[[X] =>> ScaledMaxFingerTree[X, R], A, R] = {
    val treeMeasure = tree.measure
    tree match
      case Empty() => throw new IllegalArgumentException("Cannot split an empty tree")
      case Single(value) =>
        ScaledSplit(Empty(), value, Empty())
      case Deep(prefix, middle, suffix) =>
        val leftMeasure = prefix.measure
        val middleMeasure = middle.measure
        val rightMeasure = suffix.measure
        // TODO: use equalsApprox
        if (ord.gteq(leftMeasure, treeMeasure)) then
          val digitSplit = splitMaxDigit(prefix)
          ScaledSplit(digitSplit.left.toTree, digitSplit.pivot, deepL(digitSplit.right, middle, suffix))
        else if (!middle.isEmpty && ord.gteq(middleMeasure, treeMeasure)) then
          val middleSplit = splitMax(middle)
          val digitSplit = splitMaxDigit(middleSplit.pivot.toDigit)
          ScaledSplit(deepR(prefix, middleSplit.left, digitSplit.left),
            digitSplit.pivot,
            deepL(digitSplit.right, middleSplit.right, suffix))
        else
          val digitSplit = splitMaxDigit(suffix)
          ScaledSplit(deepR(prefix, middle, digitSplit.left),
            digitSplit.pivot,
            digitSplit.right match {
              case None => Empty()
              case Some(d) => d.toTree
            })
  }


  def concat[A, R](left: ScaledMaxFingerTree[A, R], right: ScaledMaxFingerTree[A, R])(using m: Measured[A, R], rG: ScalingGroup[R], ord: Ordering[R]): ScaledMaxFingerTree[A, R] = {

    def nodes[A](l: List[ScaledValue[A, R]])(using Measured[A, R]): List[ScaledValue[Node[A, R], R]] = {
      // note that size of list is always bigger than 1, because digits from leaft and right are always at least 1 element
      l match
        case Seq(a, b) => List(Node.Node2(a, b, ord.max(a.measure, b.measure)).scaled(rG.one))
        case Seq(a, b, c) => List(Node.Node3(a, b, c, Seq(a.measure, b.measure, c.measure).max).scaled(rG.one))
        case Seq(a, b, c, d) =>
          List(Node.Node2(a, b, ord.max(a.measure, b.measure)).scaled(rG.one),
            Node.Node2(c, d, ord.max(c.measure, d.measure)).scaled(rG.one))
        case Seq(a, b, c, rest@_*) =>
          Node.Node3(a, b, c, Seq(a.measure, b.measure, c.measure).max).scaled(rG.one) +: nodes(rest.toList)
        case _ => throw new IllegalArgumentException("Invalid list for nodes creation")
    }

    def app3[A](l: ScaledMaxFingerTree[A, R], m: List[ScaledValue[A, R]], r: ScaledMaxFingerTree[A, R])(using Measured[A, R]): ScaledMaxFingerTree[A, R] = {
      l match
        case ScaledMaxFingerTree.Empty() =>
          m ++: r
        case ScaledMaxFingerTree.Single(a) =>
          a +: (m ++: r)
        case deepL: ScaledMaxFingerTree.Deep[A, R] =>
          r match
            case ScaledMaxFingerTree.Empty() =>
              l :++ m
            case ScaledMaxFingerTree.Single(b) =>
              l :++ (m :+ b)
            case deepR: ScaledMaxFingerTree.Deep[A, R] =>
              val newMiddle = app3(deepL.middle, nodes(deepL.right.toList ++ m ++ deepR.left.toList), deepR.middle)
              Deep(
                deepL.left,
                newMiddle,
                deepR.right
              )
    }

    app3(left, List.empty, right)
  }

}
