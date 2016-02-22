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

  def optionF(a: Int): Option[Int] = Some(a + 2)

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

  // copied from the 'state' chapter... execute a Gen using a fixed set of 'random' ints
  def testRunGen[A](g: Gen[A], is: List[Int]): A = g.sampler.run(FixedRNG(is))._1

  it should "work on an empty list of Gens" in {
    val seq = genMonad.sequence(Nil)
    testRunGen(seq, List(12, 13, 14)) should be (Nil)
  }

  it should "work on lists of Gens" in {
    val seq = genMonad.sequence(List.fill(3)(Gen.int))
    testRunGen(seq, List(12, 13, 14)) should be (List(12, 13, 14))
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


  behavior of "traverse - genMonad"

  def genTraverseF(a: Int) = Gen.listOfN(a, Gen.int)

  it should "work on an empty list" in {
    val tr = genMonad.traverse(Nil)(genTraverseF)
    testRunGen(tr, List(1, 2, 3)) should be (Nil)
  }

  it should "work on non-empty list to Gen[List[Int]]" in {
    val tr = genMonad.traverse(List(1, 3, 2))(genTraverseF)
    testRunGen(tr, List(4, 5, 6, 7, 8, 9, 10)) should be (List(List(4), List(5, 6, 7), List(8, 9)))
  }


  // 11.4

  behavior of "replicateN - genMonad"

  it should "generate empty lists for 0" in {
    testRunGen(genMonad.replicateM(0, Gen.int), List()) should be (List())
  }

  it should "generate lists for non-zero" in {
    testRunGen(genMonad.replicateM(3, Gen.int), List(1, 2, 3, 4)) should be (List(1, 2, 3))
  }


  // 11.5

  behavior of "replicateN - optionMonad"

  it should "generate empty lists for 0" in {
    optionMonad.replicateM(0, Some(1)) should be (Some(Nil))
  }

  it should "generate lists for non-zero" in {
    optionMonad.replicateM(3, Some(1)) should be (Some(List(1, 1, 1)))
  }


  behavior of "replicateN - listMonad"

  it should "generate empty lists for 0" in {
    listMonad.replicateM(0, List(1)) should be (List(Nil))
  }

  it should "generate lists for non-zero" in {
    listMonad.replicateM(3, List(1)) should be (List(List(1, 1, 1)))
  }


  // 11.6

  behavior of "filterM - optionMonad"

  def optionFilterF(a: Int): Option[Boolean] = Some(a).map(_ % 2 == 0)

  it should "be Some(Nil) for empty list" in {
    optionMonad.filterM(List())(optionFilterF) should be (Some(Nil))
  }

  it should "keep even numbers" in {
    optionMonad.filterM(List(1,2,3))(optionFilterF) should be (Some(List(2)))
  }

  it should "be Some(nil) for all failed" in {
    optionMonad.filterM(List(1,5,3))(optionFilterF) should be (Some(Nil))
  }


  // 11.7

  behavior of "compose - options"

  it should "combine both functions" in {
    def f(a: Int): Option[Boolean] = optionFilterF(a)
    def g(b: Boolean): Option[String] = if (b) Some("even") else Some("odd")
    optionMonad.compose(f, g)(1) should be (Some("odd"))
  }


  // 11.8

  behavior of "_flatmap - options"


  it should "return none for none" in {
    optionMonad._flatMap(None)(optionF) should be (None)
  }

  it should "return some for some" in {
    optionMonad._flatMap(Some(3))(optionF) should be (Some(5))
  }



  behavior of "_flatmap - list"

  it should "return nil for nil" in {
    listMonad._flatMap(Nil)(listF) should be (Nil)
  }

  it should "flatmap on non-empty list" in {
    listMonad._flatMap(List(1, 2, 3))(listF) should be (List("1", "2", "2", "3", "3", "3"))
  }


  // 11.10

  // compose(f, unit)               == f
  // (a: A) => flatMap(f(a))(unit)  == f
  //   replace f(a) with x... is that valid?
  // flatMap(x)(unit)               == x


  // 11.11

  behavior of "identify for lists"

  // something non trivial Int => List[Int]
  def listIdentityF(n: Int) = List.fill(n)(n)

  // need this to deal with the lazy param that compose doesn't like
  def listNonLazyUnit(i: Int) = listMonad.unit(i)

  it should "compose with second arg unit" in {
    listMonad.compose(listIdentityF, listNonLazyUnit)(5) should be (listIdentityF(5))
  }

  it should "compose with first arg unit" in {
    listMonad.compose(listNonLazyUnit, listIdentityF)(5) should be (listIdentityF(5))
  }

  // 11.12

  behavior of "join"

  it should "work on lists" in {
    listMonad.join(List(List(7))) should be (List(7))
  }


  // 11.13


  // yes, I should not be just copying these tests, by this point, I should be coming up with
  //  a way to specify them once...
// this doesn't seem to work :(
//  def testFlatMapImpl[M[_],A,B]( (ma: M[A])(f: A => M[B]) => M[B]) {
//  }



  behavior of "__flatmap - options"

  it should "return none for none" in {
    optionMonad.__flatMap(None)(optionF) should be (None)
  }

  it should "return some for some" in {
    optionMonad.__flatMap(Some(3))(optionF) should be (Some(5))
  }

  behavior of "__flatmap - list"

  it should "return nil for nil" in {
    listMonad.__flatMap(Nil)(listF) should be (Nil)
  }

  it should "flatmap on non-empty list" in {
    listMonad.__flatMap(List(1, 2, 3))(listF) should be (List("1", "2", "2", "3", "3", "3"))
  }


  // 11.14

  // hmmm...

  // for Par, associativity means the order of execution, time-wise, doesn't matter?

  // not sure for Parser... formats don't matter? But I didn't do that chapter, so I'm probably missing something.


  // 11.16

  // Gen's unit simply generates the parameter, so within compose it just echos back the value given
  // List's unit is just a List of the parameter... so again it's just an echo?

  // not sure where they are going with these questions.


  
}
