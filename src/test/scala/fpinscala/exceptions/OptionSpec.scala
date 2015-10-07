package fpinscala.exceptions

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {

  // 4.1

  behavior of "map"

  def mapF(i: Int) = i.toString

  it should "map None to None" in {
    None.map(mapF) should be (None)
  }

  it should "map Some to Some" in {
    Some(17).map(mapF) should be (Some("17"))
  }


  behavior of "flatMap"

  def flatMapF(i: Int) = if(i < 5) None else Some(i.toString)

  it should "map None to None" in {
    None.flatMap(flatMapF) should be (None)
  }

  it should "map Some to Some" in {
    Some(17).flatMap(flatMapF) should be (Some("17"))
  }

  it should "map Some to None" in {
    Some(4).flatMap(flatMapF) should be (None)
  }


  behavior of "getOrElse"

  it should "return the else from a None" in {
    None.getOrElse(27) should be (27)
  }

  it should "get from a Some" in {
    Some(17).getOrElse(27) should be (17)
  }


  behavior of "orElse"

  it should "return the Some else from a None" in {
    None.orElse(Some(27)) should be (Some(27))
  }

  it should "return the None else from a None" in {
    None.orElse(None) should be (None)
  }

  it should "return the provided Some" in {
    Some(17).orElse(Some(27)) should be (Some(17))
  }


  behavior of "filter"

  def filterF(i: Int) = i < 5

  it should "return None from a None" in {
    None.filter(filterF) should be (None)
  }

  it should "return None on a failing Some" in {
    Some(6).filter(filterF) should be (None)
  }

  it should "return Some on a passing Some" in {
    Some(3).filter(filterF) should be (Some(3))
  }

  // 4.2

  behavior of "variance"

  it should "return None for empty list" in {
    Option.variance(Seq()) should be (None)
  }

  // http://davidmlane.com/hyperstat/A16252.html
  it should "work for simple example" in {
    val xs = Seq(1.0, 2.0, 3.0)
    Option.variance(xs).getOrElse(throw new RuntimeException("fail")) should be (0.67 +- 0.01)
  }

  // http://study.com/academy/lesson/population-sample-variance-definition-formula-examples.html
  it should "work for reading speeds example" in {
    val xs = Seq(12.0, 8.0, 10.0, 10.0, 8.0, 12.0)
    Option.variance(xs).getOrElse(throw new RuntimeException("fail")) should be (2.67 +- 0.01)
  }


  // 4.3

  behavior of "map2"

  def map2f(a: Int, b: Boolean) = a.toString + b.toString

  it should "return None for both None" in {
    Option.map2(None, None)(map2f) should be (None)
  }

  it should "return None for first None" in {
    Option.map2(None, Some(false))(map2f) should be (None)
  }

  it should "return None for second None" in {
    Option.map2(Some(1), None)(map2f) should be (None)
  }

  it should "return Some for both Some" in {
    Option.map2(Some(1), Some(false))(map2f) should be (Some("1false"))
  }


  // 4.4

  behavior of "sequence"

  it should "return Some for empty list" in {
    Option.sequence(List()) should be (Some(List()))
  }

  it should "return None for list of one None" in {
    Option.sequence(List(None)) should be (None)
  }

  it should "return Some for list of one Some" in {
    Option.sequence(List(Some(1))) should be (Some(List(1)))
  }

  it should "return Some for list of two Somes" in {
    Option.sequence(List(Some(1), Some(2))) should be (Some(List(1, 2)))
  }

  it should "return None for list starting with None" in {
    Option.sequence(List(None, Some(1), Some(2))) should be (None)
  }

  it should "return None for list ending with None" in {
    Option.sequence(List(Some(1), Some(2), None)) should be (None)
  }

  it should "return None for list with None in the middle" in {
    Option.sequence(List(Some(1), None, Some(2))) should be (None)
  }


  // 4.5

  behavior of "traverse"

  def traverseF(i: Int) = if(i < 5) None else Some(i.toString)

  it should "return Some for empty list" in {
    Option.traverse(List())(traverseF) should be (Some(List()))
  }

  it should "return None for list of one thing that returns None" in {
    Option.traverse(List(1))(traverseF) should be (None)
  }

  it should "return Some for list of one thing that returns Some" in {
    Option.traverse(List(10))(traverseF) should be (Some(List("10")))
  }

  it should "return Some for list of two things that return Somes" in {
    Option.traverse(List(10, 20))(traverseF) should be (Some(List("10", "20")))
  }

  it should "return None for list starting with None" in {
    Option.traverse(List(1, 10, 20))(traverseF) should be (None)
  }

  it should "return None for list ending with None" in {
    Option.traverse(List(10, 20, 1))(traverseF) should be (None)
  }

  it should "return None for list with None in the middle" in {
    Option.traverse(List(10, 1, 20))(traverseF) should be (None)
  }

}


