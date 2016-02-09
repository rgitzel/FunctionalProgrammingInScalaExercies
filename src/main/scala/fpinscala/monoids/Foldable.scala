package fpinscala.monoids

import language.higherKinds
import fpinscala.laziness._
import fpinscala.datastructures.{Tree,Branch,Leaf}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero){(acc,a) => mb.op(acc, f(a))}
}


object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = {
    def curried(a: A) = (b: B) => f(b,a)
    foldMap(as)(curried)(Monoid.endoMonoid[B])(z)
  }

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    // copy similar bits from Monoid... can't quite seem to just call the other one, it's tied to List[A] not F[A]
    as.size match {
      case 0 =>
        mb.zero
      case 1 =>
        f(as.head)
      case n =>
        val (left, right) = as.splitAt(n/2)
        mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    // can't just pass in 'f', as foldRight expects lazy 'b' value
    as.foldRight(z){(i, acc) => f(i, acc)}

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero){(a, acc) => mb.op(f(a), acc)}

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = {
    def curried(a: A) = (b: B) => f(b,a)
    foldMap(as)(curried)(Monoid.endoMonoid[B])(z)
  }
}

object TreeFoldable extends Foldable[Tree] {
  // I don't know if these are the efficient ways, but... I'm lazy.
  
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    Tree.fold(as, f, mb.op)

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = {
    def curried(a: A) = (b: B) => f(b,a)
    foldMap(as)(curried)(Monoid.endoMonoid[B])(z)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(Monoid.endoMonoid[B])(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

