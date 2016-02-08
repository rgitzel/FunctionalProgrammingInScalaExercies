package fpinscala.monoids


import org.scalatest._
import Monoid._


class MonoidSpec extends FlatSpec with Matchers {

  // 10.1

  behavior of "intAddition monoid"

  {
    val m = intAddition

    it should "op correctly" in {
      m.op(2, 3) should be(5)
    }

    it should "use zero correctly" in {
      m.op(2, m.zero) should be(2)
    }
  }


  behavior of "intMultiplication monoid"

  {
    val m = intMultiplication

    it should "op correctly" in {
      m.op(2, 3) should be (6)
    }

    it should "zero correctly" in {
      m.op(2, m.zero) should be (2)
    }
  }


  behavior of "booleanOr monoid"

  {
    val m = booleanOr

    it should "op correctly" in {
      m.op(true, false) should be (true)
      m.op(false, false) should be (false)
    }

    it should "zero correctly" in {
      // NOTE: this is not ideal, these should be separate tests... but I'm lazy right now... using Gen/Prop later
      //  will cover things more thoroughly
      m.op(true, m.zero) should be (true)
      m.op(false, m.zero) should be (false)
      m.op(m.zero, true) should be (true)
      m.op(m.zero, false) should be (false)
      m.op(m.zero, m.zero) should be (m.zero)
    }
  }


  behavior of "booleanAnd monoid"

  {
    val m = booleanAnd

    it should "op correctly" in {
      m.op(false, false) should be (false)
      m.op(true, false) should be (false)
      m.op(true, true) should be (true)
    }

    it should "zero correctly" in {
      m.op(true, m.zero) should be (true)
      m.op(false, m.zero) should be (false)
      m.op(m.zero, m.zero) should be (m.zero)
    }
  }


  // 10.2

  behavior of "optionMonoid"

  {
    val m = optionMonoid(intAddition)

    it should "return first arg if second is zero" in {
      m.op(Some(1), m.zero) should be (Some(1))
    }

    it should "return second arg if first is zero" in {
      m.op(m.zero, Some(1)) should be (Some(1))
    }

    it should "return zero if both zero" in {
      m.op(m.zero, m.zero) should be (m.zero)
    }

    // wow the semantics of this is weird... or was until I added the Monoid[A] param

    it should "return something if both are non-zero" in {
      m.op(Some(2), Some(3)) should be (Some(5))
    }
  }




  // 10.3

  behavior of "endoMonoid"

  def int2x(n: Int) = 2 * n
  def intMinus3(n: Int) = n - 3

  {
    val m = endoMonoid[Int]

    it should "return first arg if second is zero" in {
      m.op(int2x, m.zero)(7) should be (14)
    }

    it should "return second arg if first is zero" in {
      m.op(m.zero, intMinus3)(7) should be (4)
    }

    it should "return zero if both zero" in {
      m.op(m.zero, m.zero)(8) should be (8)
    }

    it should "return both functions applied when both provided" in {
      m.op(int2x, intMinus3)(5) should be (4)
    }

    it should "return different result in different order" in {
      m.op(intMinus3, int2x)(5) should be (7)
    }
  }


  // 10.4

  // behavior of "monoids tested using Prop/Gen"

  // TODO: go re-understand how Prop and Gen work...


  // 10.5

  val foldingMonoid = stringMonoid
  val listToBeFolded = List(1, 2, 3, 4)
  val foldedList = "1234"

  behavior of "foldMap (using foldLeft)"

  it should "work normally" in {
    foldMap(listToBeFolded, foldingMonoid)(_.toString) should be (foldedList)
  }

  behavior of "foldMap (using foldRight)"

  it should "work normally" in {
    foldMapWithFoldRight(listToBeFolded, foldingMonoid)(_.toString) should be (foldedList)
  }


  behavior of "foldRight using foldMap"

  it should "work normally with zero to start" in {
    foldRight(listToBeFolded)(""){(a, acc) => a + acc} should be (foldedList)
  }

  it should "work normally with non-zero start" in {
    // to prove that the starting value doesn't get repeated throughout
    foldRight(listToBeFolded)("x"){(a, acc) => a + acc} should be (foldedList + "x")
  }


  behavior of "foldLeft using foldMap"

  it should "work normally with zero to start" in {
    foldLeft(listToBeFolded)(""){(a, acc) => a + acc} should be (foldedList.reverse)
  }

  it should "work normally with non-zero start" in {
    foldLeft(listToBeFolded)("x"){(a, acc) => a + acc} should be ("x" + foldedList.reverse)
  }



  // 10.6 - TODO


  // 10.7

  behavior of "foldMapV"

  def foldMapVFunction(i: Int) = (2 * i).toString

  it should "build empty string for empty seq" in {
    foldMapV(Vector(), stringMonoid)(foldMapVFunction) should be ("")
  }

  it should "build the right string on non-empty seq" in {
    foldMapV(Vector(1,2,3,4,5), stringMonoid)(foldMapVFunction) should be ("246810")
  }


  // 10.8 - TODO


  // 10.9


  behavior of "orderedMonoid"

  {
    val m = orderedMonoid

    it should "return first arg if second is zero" in {
      m.op(Some(1,2), m.zero) should be (Some(1,2))
    }

    it should "return second arg if first is zero" in {
      m.op(m.zero, Some(1,4)) should be (Some(1,4))
    }

    it should "return zero if both zero" in {
      m.op(m.zero, m.zero) should be (m.zero)
    }

    it should "return None if second is None" in {
      m.op(Some(1,2), None) should be (None)
    }

    it should "return None if first is None" in {
      m.op(None, Some(1,4)) should be (None)
    }

    it should "return None if both are None" in {
      m.op(None, None) should be (None)
    }

    it should "return Some if both are in order" in {
      m.op(Some(1,2), Some(3,4)) should be (Some(1,4))
    }

    it should "return Some if both are in order and share the boundary" in {
      m.op(Some(1,3), Some(3,4)) should be (Some(1,4))
    }

    it should "return None if they are not in order" in {
      m.op(Some(1,3), Some(2,4)) should be (None)
    }
  }


  behavior of "ordered"

  it should "return true for empty seq" in {
    ordered(Vector()) should be (true)
  }

  it should "return true for single item" in {
    ordered(Vector(1)) should be (true)
  }

  it should "return true for two sorted items" in {
    ordered(Vector(1, 2)) should be (true)
  }

  it should "return false for two unsorted items" in {
    ordered(Vector(3, 2)) should be (false)
  }

  val bigSeq = (-100.to(100)).toIndexedSeq

  it should "return true for big sequence both negative and positive" in {
    ordered(bigSeq) should be (true)
  }

  it should "return false for big sequence reversed" in {
    ordered(bigSeq.reverse) should be (false)
  }

  it should "return false for big sequence with one out of place" in {
    val bad = bigSeq ++ Vector(0)
    ordered(bad) should be (false)
  }

  it should "work for the extremes" in {
    ordered(Vector(Int.MinValue, 0, Int.MaxValue)) should be (true)
  }


  // 10.10

  behavior of "wcMonoid"

  {
    val m = wcMonoid

    it should "return first arg if second is zero" in {
      m.op(Stub("a"), m.zero) should be(Stub("a"))
    }

    it should "return second arg if first is zero" in {
      m.op(m.zero, Stub("a")) should be(Stub("a"))
    }

    it should "return zero if both zero" in {
      m.op(m.zero, m.zero) should be(m.zero)
    }

    it should "combine two stubs" in {
      m.op(Stub("x"), Stub("y")) should be (Stub("xy"))
    }

    it should "combine a stub on the left with a part with a left bit" in {
      m.op(Stub("x"), Part("b", 0, "c")) should be(Part("xb", 0, "c"))
    }

    it should "combine a stub on the left with a part without a left bit" in {
      m.op(Stub("x"), Part("", 0, "c")) should be(Part("x", 0, "c"))
    }

    it should "combine a stub on the right with a part with a right bit" in {
      m.op(Part("b", 0, "c"), Stub("x")) should be(Part("b", 0, "cx"))
    }

    it should "combine a stub on the right with a part without a left bit" in {
      m.op(Part("b", 0, ""), Stub("x")) should be(Part("b", 0, "x"))
    }

    it should "combine two parts with a shared word" in {
      m.op(Part("", 0, "a"), Part("b", 0, "")) should be(Part("", 1, ""))
    }

    it should "combine two parts without a shared word" in {
      m.op(Part("", 3, ""), Part("", 1, "")) should be(Part("", 4, ""))
    }
  }

  // 10.11

  behavior of "count"

  it should "return 0 for empty string" in {
    count("") should be (0)
  }

  it should "return 1 for single character" in {
    count("x") should be (1)
  }

  it should "return 1 for multiple characters without space" in {
    count("xyz") should be (1)
  }

  it should "return 2 for two words" in {
    count(" x z ") should be (2)
  }

  it should "return correct count for first book example" in {
    count("lorem ipsum do") should be (3)
  }

  it should "return correct count for second book example" in {
    count("lor sit amet, ") should be (3)
  }

  it should "return correct count with preceeding space" in {
    count(" lor sit amet, ") should be (3)
  }
}

