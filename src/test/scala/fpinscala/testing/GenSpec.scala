package fpinscala.testing


import fpinscala.state.FixedRNG
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

  it should "choose should return 1 regardless" in {
    Gen.choose(1, 2).sample.run(FixedRNG(List(345343)))._1 should be (1)
  }

  it should "choose should return half way" in {
    Gen.choose(1, 10).sample.run(FixedRNG(List(Int.MaxValue/2)))._1 should be (5)
  }

  it should "unit should what's passed in" in {
    Gen.unit(17).sample.run(FixedRNG(List(Int.MaxValue/2)))._1 should be (17)
  }

  it should "boolean should return whether random is even" in {
    Gen.boolean.sample.run(FixedRNG(List(1,2)))._1 should be (false)
    Gen.boolean.sample.run(FixedRNG(List(2,1)))._1 should be (true)
  }

  it should "listOfN should return a list" in {
    Gen.listOfN(4, Gen.boolean).sample.run(FixedRNG(List(1,2,4,7,9)))._1 should be (List(false, true, true, false))
  }
}


