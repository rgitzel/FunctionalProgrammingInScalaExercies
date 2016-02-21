package fpinscala.monoids


import fpinscala.monoids.Monoid._
import fpinscala.monoids.Prop.{Falsified, Passed, Proved}
import org.scalatest._


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

  behavior of "monoidLaws"


  def testMonoidLawsForSimpleTypes[A](m: Monoid[A], name: String, gen: Gen[A], maxSize: Int = 100, testCases: Int = 1000): Unit = {
    it should s"pass for $name" in {
      val result = monoidLaws(m, gen).run(maxSize, testCases, RNG.Simple(System.currentTimeMillis))
      result match {
        case Falsified(msg, n) =>
          println(s"Falsified $name after $n passed tests:\n $msg")
        case Passed =>
          println(s"$name passed $testCases tests")
        case Proved =>
          println(s"proved $name")
      }
      result.isFalsified should be(false)
    }
  }

  testMonoidLawsForSimpleTypes(intAddition,       "intAddition",        Gen.smallInt)
  testMonoidLawsForSimpleTypes(intMultiplication, "intMultiplication",  Gen.smallInt)
  testMonoidLawsForSimpleTypes(booleanAnd,        "booleanAnd",         Gen.boolean)
  testMonoidLawsForSimpleTypes(booleanOr,         "booleanOr",          Gen.boolean)
  testMonoidLawsForSimpleTypes(stringMonoid,      "stringMonoid",       Gen.stringN(10))

// hmmm, something about this is more complicated... need to tinker some more...

  //  def testMonoidLawsForContainerTypes[A,F[A]](m: Monoid[F[A]], name: String, gen: Gen[F[A]], maxSize: Int = 100, testCases: Int = 100): Unit = {
  //    it should s"pass for $name" in {
  //      val x = monoidLaws(m, gen).run(maxSize, testCases, RNG.Simple(System.currentTimeMillis))
  //      x match {
  //        case Falsified(msg, n) =>
  //          println(s"! Falsified after $n passed tests:\n $msg")
  //        case Passed =>
  //          println(s"+ OK, $name passed $testCases tests.")
  //        case Proved =>
  //          println(s"+ OK, proved $name property.")
  //      }
  //      x.isFalsified should be(false)
  //    }
  //  }

  //  testMonoidLawsForContainerTypes(listMonoid,     "listMonoid",         Gen.listOfN(10, Gen.smallInt))



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
    foldLeft(listToBeFolded)(""){(acc, a) => acc + a} should be (foldedList.reverse)
  }

  it should "work normally with non-zero start" in {
    foldLeft(listToBeFolded)("x"){(acc, a) => acc + a} should be ("x" + foldedList.reverse)
  }



  // 10.6 - TODO

  // d'oh! didn't write test, but wrote code?  Huh?

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

  def testWcMonoid(m: Monoid[WC])
  {
    behavior of m.getClass.toString

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

  testWcMonoid(wcMonoid)
  testWcMonoid(wcMonoid2)

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


  // 10.16

  behavior of "productMonoid"

  {
    val m = productMonoid(intAddition, stringMonoid)

    val a = (1, "a")
    val b = (2, "b")
    val ab = (3, "ab")
    val c = (3, "c")

    it should "return first arg if second is zero" in {
      m.op(a, m.zero) should be (a)
    }

    it should "return second arg if first is zero" in {
      m.op(m.zero, b) should be (b)
    }

    it should "return zero if both zero" in {
      m.op(m.zero, m.zero) should be (m.zero)
    }

    it should "combine int and string monoids" in {
      m.op(a, b) should be (ab)
    }

    it should "be associative" in {
      m.op(m.op(a, b), c) should be (m.op(a, m.op(b, c)))
    }
  }


  // 10.17

  behavior of "functionMonoid"

  {
    val m = functionMonoid[Int,String](stringMonoid)

    def repeater(n: Int) = n.toString * n
    def dashes(n: Int) = "-" * n
    def bangs(n: Int) = "!" * n

    it should "return first arg if second is zero" in {
      m.op(dashes, m.zero)(3) should be ("---")
    }

    it should "return second arg if first is zero" in {
      m.op(m.zero, dashes)(3) should be ("---")
    }

    it should "return zero if both zero" in {
      m.op(m.zero, m.zero)(12) should be (stringMonoid.zero)
    }

    it should "combine monoids" in {
      m.op(repeater, dashes)(3) should be ("333---")
    }

    it should "be associative" in {
      val f1 = m.op(m.op(repeater, bangs), dashes)
      val f2 = m.op(repeater, m.op(bangs, dashes))
      f1(3) should be (f2(3))
    }
  }


  // 10.18

  behavior of "bag"

  it should "be empty on empty input" in {
    bag(Vector()) should be (Map())
  }

  it should "work on book example" in {
    bag(Vector("a", "rose", "is", "a", "rose")) should be (Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }
}




