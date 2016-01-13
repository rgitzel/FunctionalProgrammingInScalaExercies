package fpinscala.testing


import fpinscala.state.{State, RNG, FixedRNG}
import org.scalatest._

class GenSpec extends FlatSpec with Matchers {

  // 8.3

  case class TestProp(check: Boolean) extends Prop

  behavior of "&&"

  it should "return true if both true" in {
    TestProp(true).&&(TestProp(true)) should be (true)
  }

  it should "return false if both false" in {
    TestProp(false).&&(TestProp(false)) should be (false)
  }

  it should "return false if first false" in {
    TestProp(false).&&(TestProp(true)) should be (false)
  }

  it should "return false if second false" in {
    TestProp(true).&&(TestProp(false)) should be (false)
  }


  // 8.4, 8.5

  behavior of "initial version of Gen"

  // abstract-out the rather gawd-awful implementation... and use our fake
  //  random-number generator to control things
  // TODO: this could also check that there are no values left in the FixedRNG, as I did for other assignments
  def testRun[A](g: Gen[A], is: List[Int]): A = g.sampler.run(FixedRNG(is))._1
  def testRun[A](g: Gen[A], i: Int): A = testRun(g, List(i))


  it should "choose should return 1 regardless" in {
    testRun(Gen.choose(1, 2), 345343) should be (1)
  }

  it should "choose should return half way" in {
    testRun(Gen.choose(1, 10), Int.MaxValue/2) should be (5)
  }

  it should "unit should what's passed in" in {
    testRun(Gen.unit(17), Int.MaxValue/2) should be (17)
  }

  it should "boolean should return whether random is even" in {
    testRun(Gen.boolean, 1) should be (false)
    testRun(Gen.boolean, 2) should be (true)
  }

  it should "listOfN should return a list" in {
    testRun(Gen.listOfN(4, Gen.boolean), List(1,2,4,7,9)) should be (List(false, true, true, false))
  }


  // after 8.5

  behavior of "char"

  it should "generate an 'a'" in {
    testRun(Gen.char, 0) should be ('a')
  }

  it should "generate an 'z'" in {
    testRun(Gen.char, 51) should be ('z')
  }



  behavior of "string"

  it should "generate an empty string on lenght 0" in {
    testRun(Gen.string(0), 1) should be ("")
  }

  it should "generate a single-character string on length 1" in {
    testRun(Gen.string(1), 1) should be ("b")
  }

  it should "generate a 3-character string on length 3" in {
    testRun(Gen.string(3), List(1, 0, 17)) should be ("bar")
  }


  behavior of "map"

  it should "turn an int into a string" in {
    val g = Gen.unit(7).map{ _.toString }
    testRun(g, 1) should be ("7")
  }


  // 8.6

  behavior of "flatMap"

  it should "turn an int into a string of that length" in {
    val g = Gen.choose(1,6).flatMap{ i => Gen.string(i) }
    // first 'random' will determine the length (mid of 1-5, so 3), then need that many more
    testRun(g, List(Int.MaxValue/2, 1, 0, 17)) should be ("bar")
  }

  behavior of "sequence"

  // c.f. StateSpec

  it should "can combine 3 functions" in {
    val g = Gen.sequence(List(Gen(State(RNG.nonNegativeEven)), Gen(State(RNG.nonNegativeInt _)), Gen(State(RNG.nonNegativeEven))))
    testRun(g, List(17, 5, 43, 12)) should be (List(16, 5, 42))
  }


  behavior of "second listOfN"

  it should "turn an int into a list of that many chars" in {
    val g = Gen.char.listOfN(Gen.unit(3))
    testRun(g, List(1, 0, 17)) should be (List('b', 'a', 'r'))
  }


  // 8.7

  behavior of "union"

  val unionGen = Gen.union(Gen(State(RNG.nonNegativeEven)), Gen(State(RNG.nonNegativeOdd)))

  it should "pick even when true" in {
    testRun(unionGen, List(2, 5)) should be (4)
  }

  it should "pick odd when false" in {
    testRun(unionGen, List(1, 5)) should be (5)
  }


  // 8.8

  behavior of "weighted"

  // even is three times more likely
  val weightedGen = Gen.weighted(
    (Gen(State(RNG.nonNegativeEven)), 0.75),
    (Gen(State(RNG.nonNegativeOdd)), 0.25)
  )

  val threshold = (Int.MaxValue * .75).toInt

  it should "pick even when under 75%" in {
    testRun(weightedGen, List(threshold - 1, 5)) should be (4)
  }

  it should "pick odd when over 75%" in {
    testRun(weightedGen, List(threshold + 1, 5)) should be (5)
  }
}


