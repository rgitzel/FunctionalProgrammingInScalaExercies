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

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    // well that was really hard to sort out, and rather simple, ultimately... still don't quite understand it :(
    val s = RNG.flatMap[A,B](sample.run){ a =>
      f(a).sample.run
      // it seems odd to have to 'unroll' it, but otherwise we end up with Rand[Gen[B]] and then Gen[Gen[B]]
    }
    Gen(State(s))
  }
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

  def char: Gen[Char] = {
    val r = RNG.map(RNG.int)(i => ('a'.toInt + (i % 26)).toChar)
    Gen(State(r))
  }

  def string(len: Int): Gen[String] = {
    val chars = 1.to(len).map(i => char.sample.run)
    val s = RNG.flatMap[List[Char],String](RNG.sequence(chars.toList)){ cs =>
      RNG.unit(cs.mkString)
    }
    Gen(State(s))
    // I think I just implemented a specific version of flatmap...
  }
}



trait SGen[+A] {

}

