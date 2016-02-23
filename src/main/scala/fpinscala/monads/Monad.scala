package fpinscala.monads

import fpinscala.parallelism.Par.Par
import fpinscala.state.State
import fpinscala.testing.Gen
import language.higherKinds
import fpinscala.laziness.Stream


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List[A]())){ (mla, ma) =>
      flatMap(mla){ list =>
        map(ma)(list :+ _)
      }
    }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    sequence(la.map(f))


  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: M[A], mb: M[B]) = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldLeft(unit(List[A]())){ case(mla, a) =>
      flatMap(f(a)){ b =>
        if(b)
          map(mla)(_ :+ a)
        else
          mla
      }
    }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => flatMap(f(a)){ b => g(b) }

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = {
// 1st solution... which cheats because it uses flatmap! and it seems weak to use identity for one of the composed functions
//    def x(a: A) = compose((a: A) => unit(a), f)(a)
//    flatMap(ma)(x)

    // we have M[A], how the heck to do we take it apart to get the A??!?!?
    //  lightbulb!  look at compose signature... it "knows" how to take the output from f and feed it
    //  to g, which means it "knows" how to turn M[B] to B!  We don't know or care *how*, it just does...
    // so make the first function return ma, regardless of input... but the input needs to be SOMEthing
    //  so make it an Int for sake of argument (and no pun intended, the argument value can be anything too)
    def f1(n: Int) = ma
    compose[Int,A,B](f1, f)(1) // don't need the type signature, just putting it in for clarity
  }

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma) { ma =>
      // really? that's it?
      ma
    }

   // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma){ a => f(a) })
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

//  val parMonad: Monad[Par] = ???

//  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) =
      ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]) =
      ma flatMap f
  }

// nope...
//  def stateMonad[S,A] = new Monad[State[S,A]] {
//    def unit[A](a: => A): List[A] = List(a)
//    def flatMap[A,B](ma: List[A])(f: A => List[B]) =
//      ma flatMap f
//  }

  // copied from book
  def stateMonad[S,A] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]) =
      ma flatMap f
  }

//  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = ???
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = ???
  }
}

