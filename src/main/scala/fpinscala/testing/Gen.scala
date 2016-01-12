package fpinscala.testing

import fpinscala.state.{RNG, State}


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean
  def &&(p: Prop): Boolean = check && p.check
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG,A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val r = RNG.map(RNG.double)(d => start + (d * (stopExclusive - start)).toInt)
    Gen(State(r))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    val r = RNG.map(RNG.int)(i => i % 2 == 0)
    Gen(State(r))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    // I really don't understand what I'm doing here... just letting the compiler guide me :(
    val gs = 1.to(n).map(i => g.sample.run)
    Gen(State(RNG.sequence(gs.toList)))
  }

}



trait SGen[+A] {

}

