package fpinscala.testing

import fpinscala.state.{RNG, State}


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait FirstPropImpl {
  def check: Boolean
  // TODO: crap! turns out I did this wrong :p it's supposed to return Prop
  def &&(p: FirstPropImpl): Boolean = check && p.check
}

object FirstPropImpl {
  def forAll[A](gen: Gen[A])(f: A => Boolean): FirstPropImpl = ???
}

case class Gen[A](sampler: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = Gen(sampler.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    // well that was really hard to sort out, and rather simple, ultimately... still don't quite understand it :(
//    val s = RNG.flatMap[A,B](sample.run){ a =>
//      f(a).sample.run
//      // it seems odd to have to 'unroll' it, but otherwise we end up with Rand[Gen[B]] and then Gen[Gen[B]]
//    }
//    Gen(State(s))

    // much simpler... though how does the same RNG end up being used in both cases? State/RNG propogates it at 'run' time?
    Gen(sampler.flatMap(f(_).sampler))
  }

  def listOfN(len: Gen[Int]): Gen[List[A]] = {
    val x = len.flatMap{ n =>
      Gen.sequence(List.fill(n)(Gen(sampler)))
    }
    x
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val r = RNG.map(RNG.double)(d => start + (d * (stopExclusive - start)).toInt)
    Gen(State(r))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    // could have added .boolean to RNG instead
    val r = RNG.map(RNG.int)(i => i % 2 == 0)
    Gen(State(r))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    // I really don't understand what I'm doing here... just letting the compiler guide me :(
    val gs = 1.to(n).map(i => g.sampler.run)
    Gen(State(RNG.sequence(gs.toList)))
  }

  def char: Gen[Char] = {
    val r = RNG.map(RNG.int)(i => ('a'.toInt + (i % 26)).toChar)
    Gen(State(r))
  }

  def string(len: Int): Gen[String] = {
    val chars = 1.to(len).map(i => char.sampler.run)
    val s = RNG.flatMap[List[Char],String](RNG.sequence(chars.toList)){ cs =>
      RNG.unit(cs.mkString)
    }
    Gen(State(s))
    // I think I just implemented a specific version of flatmap...
  }

  def sequence[A](gs: List[Gen[A]]): Gen[List[A]] = {
    gs.foldLeft(unit(List[A]())) { case (genListASoFar, genA) =>
      //      genSoFar.flatMap { list =>
      //        // @#$@#$ how combine a List[A] with a Gen[A]?!?!
      //        // INSIGHT... just keep flatmapping until we've pulled out all the values for each piece
      //        g.flatMap{ a =>
      //          Gen.unit(list :+ a)
      //        }
      //      }

//      for {
//        listSoFar <- genListASoFar
//        a <- genA
//      }
//      yield(listSoFar :+ a)

      genListASoFar.flatMap(listSoFar =>
        genA.map(a => listSoFar :+ a)
      )
    }
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if(b) g1 else g2)

  def double: Gen[Double] = Gen(State(RNG.double))

  // for simplicity's sake, I'm expecting that the two weights add up to 1.0
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    double.flatMap( d => if(d < g1._2) g1._1 else g2._1)
  }
}



trait SGen[+A] {

}

