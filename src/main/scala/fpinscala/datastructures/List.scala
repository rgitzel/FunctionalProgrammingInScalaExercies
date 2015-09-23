package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(oldHead, tail) => Cons(h, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if(n > 1)
          drop(tail, n-1)
        else
          tail
    }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if(f(head))
          dropWhile(tail, f)
        else
          l
    }


  def init[A](l: List[A]): List[A] =
    l match {
      case Nil              => Nil
      case Cons(head, Nil)  => List(head)
      case Cons(head, tail) => init(tail)
    }

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  // 3.10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  // 3.11

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A]){ (b, a) => Cons(a, b) }

  // 3.13

  def foldRightUsingLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a, b))

  def foldLeftUsingRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a,b) => f(b, a))

  // 3.14
  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((item, list) => Cons(item, list))

  // 3.15
  def concatLists[A](lists: List[List[A]]): List[A] =
    foldRight[List[A],List[A]](lists, Nil){(list, resultList) =>
      appendWithFold(list, resultList)
    }

  // 3.16
  def addOne(is: List[Int]): List[Int] =
    foldRight(is, Nil: List[Int])((i, list) => Cons(i+1, list))

  // 3.17
  def doublesToStrings(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((d, list) => Cons(d.toString, list))

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, list) => Cons(f(a), list))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, list) => if(f(a)) Cons(a, list) else list)

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, list) => append(f(a), list))

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] =
    concatLists[B](map[A,List[B]](as)(f))

  // 3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  // 3.22... going to assume lists of the same size for sake of the exercise
  def addingCorrespondingLists(list1: List[Int], list2: List[Int]): List[Int] =
    // this seems... not right... it depends on matching pairs in Scala, which isn't in the book yet?
    //  but I don't think there's a way to traverse two lists in parallel with maps or folds;
    //  I guess you could build the list of pairs, first
    (list1, list2) match {
      case (Nil, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addingCorrespondingLists(t1, t2))
      case _ => throw new IllegalArgumentException("lists should be same size")
    }

  def zipWith[A,B,C](list1: List[A], list2: List[B], f: (A,B) => C): List[C] =
    (list1, list2) match {
      case (Nil, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
      case _ => throw new IllegalArgumentException("lists should be same size")
    }
}