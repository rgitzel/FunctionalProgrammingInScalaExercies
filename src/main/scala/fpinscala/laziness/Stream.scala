package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = 
    this match {
      case Empty => Nil
      case Cons(h, t) => List(h()) ++ t().toList
    }

  def take(n: Int): Stream[A] = 
    this match {
      case Empty => 
       	Empty
      case Cons(h, t) => 
        if(n <= 0)
          Empty
        else
          cons(h(), t().take(n-1))
    }

  def take2(n: Int): Stream[A] =
    // ug!
    unfold((n, this)){ case(i, underlying) =>
      underlying match {
        case Empty =>
          None
        case Cons(h, t) =>
          if(i <= 0)
            None
          else
            Some(h(), (i - 1, t()))
      }
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Empty =>
        Empty
      case Cons(h, t) => 
        if(n <= 0)
          Cons(h, t)
        else
          t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => // we don't have .map yet, otherwise we wouldn't need this?
 	      Empty
      case Cons(h, t) =>
        if(!p(h()))
          Empty
        else
          cons(h(), t().takeWhile(p))
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty){ (a,b) =>
      if(p(a))     
	      cons(a, b.takeWhile2(p))
      else
        empty
    }


  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) =>
        val v = h()
        if(p(v))
          Some(v, t())
        else
          None
      case _ =>
        None
    }

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)    

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def map2[B](f: A => B): Stream[B] =
    // rather awkward...
    unfold(this) {
      case Cons(h, t) =>
        Some(f(h()), t())
      case _ =>
        None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) =>
      if(p(a))
        cons(a, b.filter(p))
      else
        b.filter(p)
    )

  // odd, this bit of type-class hoogijiggery is out of scope at this point in the book?
  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other)((a,b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a).append(b))

  def zipWith[B, C](bs: => Stream[B], f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) =>
        val fv = f(ha(), hb())
        Some(fv, (ta(), tb()))
      case _ =>
        None
    }

  def zipAll[B, C](bs: => Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) =>
        val v = (Some(ha()), Some(hb()))
        Some(v, (ta(), tb()))
      case (Cons(ha, ta), _) =>
        val v = (Some(ha()), None)
        Some(v, (ta(), empty))
      case (_, Cons(hb, tb)) =>
        val v = (None, Some(hb()))
        Some(v, (empty, tb()))
      case _ =>
        None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).forAll {
      case (Some(a), Some(b)) =>
        a == b
      case (Some(a), None) =>
        true
      case _ =>
        false
    }

  def tails: Stream[Stream[A]] =
    Stream(this).append(
      unfold(this) {
        case Cons(h, t) =>
          Some(t(), t())
        case _ =>
          None
      }
    )

  def scanRightUsingFoldRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(empty[B]){ case(a,b) =>
      b match {
        case Empty =>
          cons(f(a, z), Stream(z))
        case Cons(h, t) =>
          cons(f(a, h()), b)
      }
    }

// not only is more complicated, using a tuple, but... unfold is not going to
//  work... if it was scanLEFT then yes, but... we're going at it in reverse, and we
//  can't reverse a stream... can we?

  def scanRightUsingUnfold[B](z: => B)(f: (A, => B) => B): Stream[B] =
    unfold[B,Tuple2[Stream[A],Stream[B]]]((this, empty[B])){ case(as, interimResults) =>
      as match {
        case Cons(a, t) =>
          interimResults match {
            case Empty =>
              Some(z, (as, Stream(z)))
            case c =>
              val v = f(a(), interimResults.headOption.get)
              Some(v, (t(), cons(v, interimResults)))
          }
        case _ =>
          None
      }
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(n: Int): Stream[Int] = cons(n, constant(n))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(n1: Int, n2: Int): Stream[Int] = {
      val sum = n1 + n2
      cons(sum, f(n2, sum))
    }
    Stream(0, 1).append(f(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((nextItem, nextZ)) =>
        cons(nextItem, unfold(nextZ)(f))
      case _ =>
        empty
    }

  def fibs2: Stream[Int] =
    Stream(0, 1).append(
      unfold((0,1)){ case(n1, n2) =>
        val sum = n1 + n2
        Some(sum, (n2, sum))
      }
    )

  def from2(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  def constant2(n: Int): Stream[Int] = unfold(n)(i => Some(i, i))

  def ones2: Stream[Int] = unfold(1)(i => Some(1,1))

}
