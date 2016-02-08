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
    foldRight(listToBeFolded)("= "){(a, acc) => a + acc} should be ("= " + foldedList)
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

  val bigSeq = (1.to(100)).toIndexedSeq

  it should "return true for big sequence" in {
    ordered(bigSeq) should be (true)
  }

  it should "return false for big sequence reversed" in {
    ordered(bigSeq.reverse) should be (false)
  }

  it should "return false for big sequence with one out of place" in {
    val bad = bigSeq ++ Vector(0)
    ordered(bad) should be (false)
  }
}




