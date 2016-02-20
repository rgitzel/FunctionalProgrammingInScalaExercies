package fpinscala.monads

import fpinscala.monads.Monad._
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
}
