package fpinscala.applicative

import org.scalatest._
import ApplicativeByApply._


class ApplicativeByApplySpec extends FlatSpec with Matchers {


  // 12.2

  behavior of "applicativeByApply.map"

  it should "map None to None" in {
    optionApplicativeByApply.map[Int,Int](None)(_ + 1) should be (None)
  }

  it should "map Some to Some" in {
    optionApplicativeByApply.map(Some(1))(_ + 1) should be (Some(2))
  }

  behavior of "applicativeByApply.map2"

  def sum(a: Int, b: Int) = a + b

  it should "map2 two Somes to a Some" in {
    optionApplicativeByApply.map2(Some(1), Some(2))(sum) should be (Some(3))
  }

  it should "map2 Some/None to a None" in {
    optionApplicativeByApply.map2(Some(1), None)(sum) should be (None)
  }

  it should "map2 None/Some to a None" in {
    optionApplicativeByApply.map2(None, Some(1))(sum) should be (None)
  }

}
