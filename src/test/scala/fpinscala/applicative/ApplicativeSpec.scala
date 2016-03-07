package fpinscala.applicative

import org.scalatest._
import Applicative._

class ApplicativeSpec extends FlatSpec with Matchers {

//  behavior of "listApplicative"
//
//  it should "map2 correctly" in {
//    listApplicative.map2(List(1, 2), List(2, 3))(_ + _) should be (List(3, 5))
//  }


  behavior of "optionApplicative"

  def sum(a: Int, b: Int) = a + b

  it should "map2 two Somes to a Some" in {
    optionApplicative.map2(Some(1), Some(2))(sum) should be (Some(3))
  }

  it should "map2 Some/None to a None" in {
    optionApplicative.map2(Some(1), None)(sum) should be (None)
  }

  it should "map2 None/Some to a None" in {
    optionApplicative.map2(None, Some(1))(sum) should be (None)
  }


  // 12.1


  behavior of "sequence for options"

  it should "convert List[Option] to Option[List]" in {
    optionApplicative.sequence(List(Some(1), Some(2), Some(0))) should be (Some(List(1, 2, 0)))
  }

  it should "convert empty list to option of empty list" in {
    optionApplicative.sequence(Nil) should be (Some(Nil))
  }


  behavior of "replicatedM for options"

  it should "convert an option to a list of that option" in {
    val o = Some(1)
    optionApplicative.replicateM(4, o) should be (Some(List(1, 1, 1, 1)))
  }

  it should "convert option to empty list" in {
    optionApplicative.replicateM(0, Some(1)) should be (Some(Nil))
  }


  behavior of "product for options"

  it should "combine two options" in {
    optionApplicative.product(Some(1), Some(2)) should be (Some((1, 2)))
  }


  // 12.2

  behavior of "apply - options"

  def aToB(a: Int) = (a+1).toString

  it should "work" in {
    // weird, either I have to put the '_' after aToB, or put [Int,String] after 'apply'
    optionApplicative.apply(Some(aToB _))(Some(6)) should be (Some("7"))
  }




  // 12.3

  behavior of "map3 for options"

  def sum3(a: Int, b: Int, c: Int) = a + b + c

  it should "combine three somes" in {
    optionApplicative.map3(Some(1), Some(2), Some(3))(sum3) should be (Some(6))
  }


  behavior of "map4 for options"

  def sum4(a: Int, b: Int, c: Int, d: Int) = a + b + c + d

  it should "combine four somes" in {
    optionApplicative.map4(Some(1), Some(2), Some(3), Some(4))(sum4) should be (Some(10))
  }

}
