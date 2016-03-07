package fpinscala.applicative

import fpinscala.applicative.Validation._
import org.scalatest._


class ValidationSpec extends FlatSpec with Matchers {


  // 12.6

  behavior of "map2 for Validation"

  def sum(a: Int, b: Int) = a + b

  it should "combine two successes" in {
    validationApplicative.map2(Success(1), Success(2))(sum) should be(Success(3))
  }


  val f1 = Failure("ack", Vector("foo", "bar"))
  val f2 = Failure("bad", Vector("dog"))


  it should "keep the failure if only left fails" in {
    validationApplicative.map2(f1, Success(2))(sum) should be(f1)
  }

  it should "keep the failure if only right fails" in {
    validationApplicative.map2(Success(1), f2)(sum) should be(f2)
  }

  it should "combine the failures if both fail" in {
    val expected = Failure(
      f1.head,
      Vector(f2.head) ++ f1.tail ++ f2.tail
    )
    validationApplicative.map2(f1, f2)(sum) should be(expected)
  }
}

