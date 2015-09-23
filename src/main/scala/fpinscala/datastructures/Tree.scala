package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // 3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

  // 3.26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(value) => value
      case Branch(left, right) => Math.max(maximum(left), maximum(right))
    }

  // 3.27 assuming the first node is path length 1
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(value) => 1  // assumption
      case Branch(left, right) => 1 + Math.max(depth(left), depth(right))
    }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value) =>
        Leaf(f(value))
      case Branch(left, right) =>
        Branch(map(left)(f), map(right)(f))
    }

  // 3.29... the trick seems to be you need to provide TWO functions, one
  //  to do something with the A value of a leaf, and one to do something to "combine"
  //  the B values of the two subtrees of a branch... you could probably combine
  //  them, but then you might be tempted to recurse? oh! isn't there a "fold" in Play's
  //  form processing somewhere that has alternate handlers, one for error, one for success?
  //  that seems the same idea... originally I made three parameters lists to make type inference
  //  easier, but all the extra ()'s made the fold calls harder to read... explicitly adding
  //  the types to each call isn't great either, but looks better
  def fold[A,B](t: Tree[A], z: A => B, combine: (B,B) => B): B =
    t match {
      case Leaf(value) =>
        z(value)
      case Branch(left, right) =>
        combine(fold(left, z, combine), fold(right, z, combine))
    }

  def sizeWithFold[A](t: Tree[A]): Int = fold[A,Int](t, _ => 1, _ + _ + 1)

  def maximumWithFold(t: Tree[Int]): Int = fold[Int,Int](t, v => v, _.max(_))

  def depthWithFold[A](t: Tree[A]): Int = fold[A,Int](t, _ => 1, _.max(_) + 1)

  def mapWithFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A,Tree[B]](
      t,
      v => Leaf(f(v)),
      Branch(_, _)
    )
}