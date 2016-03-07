package fpinscala.applicative

import fpinscala.monads.Functor
import fpinscala.monoids.Monoid

import scala.language.{higherKinds, implicitConversions}

/*
 * an alternate version for exercise 12.2
 */
trait ApplicativeByApply[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    val fab = unit(f)
    apply(fab)(fa) // interesting, get compile error if 'unit(f)' is passed to apply() directly
  }

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fBToC = apply[A,B=>C](unit(f.curried))(fa)
    apply(fBToC)(fb)
  }

}

object ApplicativeByApply {
  val optionApplicativeByApply = new ApplicativeByApply[Option] {
    def unit[A](a: => A) = Some(a)

    def apply[A,B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      (fab, fa) match {
        case (Some(aToB), Some(a)) => Some(aToB(a))
        case _ => None
      }
  }

}
