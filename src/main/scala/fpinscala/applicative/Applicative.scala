package fpinscala.applicative

import fpinscala.monads.{Monad, Functor}
import fpinscala.state.State

import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    // start with F[A] and F[B]
    val fcd = map2(fa, fb){ case(a, b) =>
      f.curried(a)(b)
    }
    // now we have F[C=>D] and F[C]
    map2(fcd, fc){ case (cToD, c) =>
      cToD(c)
    }
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fde = map3(fa, fb, fc){ case(a, b, c) =>
      f.curried(a)(b)(c)
    }
    // now we have F[D=>E] and F[D]
    map2(fde, fd){ case (dToE, d) =>
      dToE(d)
    }
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    // weird-looking syntax, yes...
    map2(fab, fa){ _(_) }

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List[A]())){ (acc, a) =>
      map2(acc, a)(_ :+ _)
    }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)( (_, _) )

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if(n == 0)
      unit(Nil)
    else
      map2(fa, replicateM(n - 1, fa))(_ +: _)


  // okay wait what? how is this different from previous 'product' other than fancy signature?
  //  I think they missed a step somewhere
  //def productFancy[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x],G[x])})#f] = ???


  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = ???

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.toList.foldLeft(unit(Map[K,V]())){ case (fm, (k, fv)) =>
        map2(fm, fv){ case (m, v) =>
          m ++ Map(k -> v)
        }
      }
}

object Applicative {
  val listApplicative = new Applicative[List] {
    def unit[A](a: => A) = List(a)
    def map2[A,B,C](fa: List[A], fb: List[B])(f: (A, B) => C) =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }

  val optionApplicative = new Applicative[Option] {
    def unit[A](a: => A) = Some(a)
    def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A, B) => C) =
      for {
        a <- fa
        b <- fb
      } yield f(a, b)
  }


  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                          f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }


  //  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = ???
  //
  //  type Const[A, B] = A
  //
  //  implicit def monoidApplicative[M](M: Monoid[M]) =
  //    new Applicative[({ type f[x] = Const[M, x] })#f] {
  //      def unit[A](a: => A): M = M.zero
  //      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
  //    }

}


//case class Tree[+A](head: A, tail: List[Tree[A]])
//
//trait Monad[F[_]] extends Applicative[F] {
//  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))
//
//  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
//
//  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
//    a => flatMap(f(a))(g)
//
//  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
//    flatMap(mf)(f => map(ma)(a => f(a)))
//}


object EitherMonad {
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A): Either[E, A] = Right(a)
    def flatMap[A,B](either: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      either match {
        case Right(a) =>
          f(a)
        case Left(e) =>
          Left(e)
      }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
//
//  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
//  Monad[({type f[x] = F[N[x]]})#f] = ???
}


//trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
//  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
//    sequence(map(fa)(f))
//  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
//    traverse(fma)(ma => ma)
//
//  type Id[A] = A
//  val idMonad = new Monad[Id] {
//    def unit[A](a: => A) = a
//    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
//  }
//
//  def map[A,B](fa: F[A])(f: A => B): F[B] =
//    traverse[Id, A, B](fa)(f)(idMonad)
//
//  import Applicative._
//
//  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
//    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
//      as)(f)(monoidApplicative(mb))
//
//  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
//    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)
//
//  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
//    traverseS(fa)((a: A) => (for {
//      s1 <- get[S]
//      (b, s2) = f(a, s1)
//      _  <- set(s2)
//    } yield b)).run(s)
//
//  override def toList[A](fa: F[A]): List[A] =
//    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
//
//  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
//    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1
//
//  def reverse[A](fa: F[A]): F[A] = ???
//
//  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???
//
//  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
//                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???
//
//  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
//}
//
//object Traverse {
//  val listTraverse = ???
//
//  val optionTraverse = ???
//
//  val treeTraverse = ???
//}
//
//// The `get` and `set` functions on `State` are used above,
//// but aren't in the `exercises` subproject, so we include
//// them here
//object StateUtil {
//
//  def get[S]: State[S, S] =
//    State(s => (s, s))
//
//  def set[S](s: S): State[S, Unit] =
//    State(_ => ((), s))
//}
