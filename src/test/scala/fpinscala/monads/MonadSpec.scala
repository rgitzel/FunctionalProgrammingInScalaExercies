package fpinscala.monads

import fpinscala.monads.Monad._
import fpinscala.testing.Gen
import fpinscala.state.FixedRNG
import org.scalatest._
import fpinscala.laziness.Stream


class MonadSpec extends FlatSpec with Matchers {

  // 11.1

  behavior of "Monad[Par]"

//  it should "empty flatmap" in {
//    parMonad.flatMap()op(2, 3) should be(5)
//  }
//
//  it should "use zero correctly" in {
//    m.op(2, m.zero) should be(2)
//  }


  behavior of "Monad[Option]"

  val optionF = (a: Int) => Some(a + 2)

  it should "build a some with unit" in {
    optionMonad.unit(23) should be (Some(23))
  }

  it should "flatmap on None" in {
    optionMonad.flatMap(None)(optionF) should be (None)
  }

  it should "flatmap on non-empty list" in {
    optionMonad.flatMap(Some(3))(optionF) should be (Some(5))
  }


  behavior of "Monad[Stream]"

  val streamF = (a: Int) => Stream(a + 1, a + 2)

  it should "build a stread with unit" in {
    streamMonad.unit(23).take(2).toList should be (List(23))
  }

  it should "flatmap on empty stream" in {
    streamMonad.flatMap(Stream.empty)(streamF).take(1).toList should be (Nil)
  }

  it should "flatmap on non-empty stream" in {
    streamMonad.flatMap(Stream(1,2,3))(streamF).take(10).toList should be (List(2, 3, 3, 4, 4, 5))
  }


  behavior of "Monad[List]"

  val listF = (a: Int) => List.fill(a)(a.toString)

  it should "build a list with unit" in {
    listMonad.unit(23) should be (List(23))
  }

  it should "flatmap on empty list" in {
    listMonad.flatMap(Nil)(listF) should be (Nil)
  }

  it should "flatmap on non-empty list" in {
    listMonad.flatMap(List(1,2,3))(listF) should be (List("1", "2", "2", "3", "3", "3"))
  }


  // 11.3

  behavior of "sequence"

  def testRun[A](g: Gen[A], is: List[Int]): A = g.sampler.run(FixedRNG(is))._1

  it should "work on an empty list of Gens" in {
    val seq = genMonad.sequence(Nil)
    testRun(seq, List(12, 13, 14)) should be (Nil)
  }

  it should "work on lists of Gens" in {
    val seq = genMonad.sequence(List.fill(3)(Gen.int))
    testRun(seq, List(12, 13, 14)) should be (List(12, 13, 14))
  }

  it should "return Some on an empty list of Options" in {
    optionMonad.sequence(Nil) should be (Some(Nil))
  }

  it should "return Some on lists of all Somes" in {
    optionMonad.sequence(List(Some(12), Some(13), Some(14))) should be (Some(List(12, 13, 14)))
  }

  it should "return None on lists a None" in {
    optionMonad.sequence(List(Some(12), None, Some(14))) should be (None)
  }


  behavior of "traverse - optionMonad"

  def optionTraverseF(a: Int) = if(a % 2 == 0) Some(a) else None

  it should "return Some(Nil) on Nil" in {
    optionMonad.traverse(Nil)(optionTraverseF) should be (Some(Nil))
  }

  it should "return Some if all ints map to Some" in {
    optionMonad.traverse(List(2, 6, 8))(optionTraverseF) should be (Some(List(2, 6, 8)))
  }

  it should "return None if at least one int map to None" in {
    optionMonad.traverse(List(2, 6, 9))(optionTraverseF) should be (None)
  }
}
