package fpinscala.applicative



sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A)
  extends Validation[Nothing, A]



object Validation {
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    def unit[A](a: => A) = Success(a)

    def map2[A,B,C](va: Validation[E,A], vb: Validation[E,B])(f: (A, B) => C) =
      (va, vb) match {
        case (Success(a), Success(b)) =>
          Success(f(a,b))

        case (Failure(ea, ta), Failure(eb, tb)) =>
          // for sake of the exercise, keep the new errors at the head
          Failure(ea, eb +: (ta ++ tb))

        case (Failure(ea, te), _) =>
          Failure(ea, te)

        case (_, Failure(eb, tb)) =>
          Failure(eb, tb)
      }
  }
}