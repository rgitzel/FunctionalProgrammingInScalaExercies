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

  // 3.29
}