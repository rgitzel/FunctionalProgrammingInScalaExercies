package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  behavior of "x"

  it should "3" in {
    List.x should be (3)
  }


  behavior of "tail"

  it should "be Nil for Nil" in {
    List.tail(Nil) should be (Nil)
  }

  it should "be Nil for single-item list" in {
    List.tail(List(1)) should be (Nil)
  }

  it should "be the tail for list with a tail" in {
    List.tail(List(1, 2)) should be (List(2))
  }



  behavior of "setHead"

  it should "be Nil for Nil" in {
    List.setHead(Nil, 1) should be (Nil)
  }


  it should "work for single-item list" in {
    List.setHead(List(1), 2) should be (List(2))
  }

  it should "work for list with a tail" in {
    List.setHead(List(1, 2), 3) should be (List(3, 2))
  }




  behavior of "drop"

  it should "be Nil for Nil" in {
    List.drop(Nil, 1) should be (Nil)
  }


  it should "work for single-item list" in {
    List.drop(List(1), 1) should be (Nil)
  }

  it should "work for list with a tail" in {
    List.drop(List(1, 2), 1) should be (List(2))
  }

  it should "remove the whole list is N > size" in {
    List.drop(List(1, 2), 3) should be (Nil)
  }


  behavior of "dropWhile"

  it should "be Nil for Nil" in {
    List.dropWhile[Int](Nil, _ > 5) should be (Nil)
  }


  it should "filter single-item list" in {
    List.dropWhile[Int](List(1), _ < 5) should be (Nil)
  }

  it should "not filter single-item list" in {
    List.dropWhile[Int](List(7), _ < 5) should be (List(7))
  }

  it should "stop removing after a couple" in {
    List.dropWhile[Int](List(1, 2, 3, 4, 5), _ < 3) should be (List(3, 4, 5))
  }



  behavior of "init"

  it should "be Nil for Nil" in {
    List.init(Nil) should be (Nil)
  }

  it should "be last item for single-item list" in {
    List.init(List(1)) should be (List(1))
  }

  it should "return last item on list > 2" in {
    List.init(List(1, 2, 3, 4, 5)) should be (List(5))
  }


  behavior of "foldRight and constructors"

  it should "return itself" in {
    List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)) should be (List(1, 2, 3))
  }


  behavior of "length"

  it should "return 0 on empty list" in {
    List.length(Nil) should be (0)
  }

  it should "return 1 for a single-item list" in {
    List.length(List(1)) should be (1)
  }

  it should "return 3 for a three-item list" in {
    List.length(List(1, 2, 3)) should be (3)
  }


  behavior of "sumLeft"

  it should "return 0 for empty list" in {
    List.sumLeft(Nil) should be (0)
  }

  it should "properly sum a single-item list" in {
    List.sumLeft(List(7)) should be (7)
  }

  it should "properly sum a three-item list" in {
    List.sumLeft(List(2,3,4)) should be (9)
  }



  behavior of "productLeft"

  it should "return 1.0 for empty list" in {
    List.productLeft(Nil) should be (1.0)
  }

  it should "properly product a single-item list" in {
    List.productLeft(List(7)) should be (7.0)
  }

  it should "properly calculate a three-item list" in {
    List.productLeft(List(2,3,4)) should be (24.0)
  }


  behavior of "lengthLeft"

  it should "return 0 for empty list" in {
    List.lengthLeft(Nil) should be (0)
  }

  it should "return 1 a single-item list" in {
    List.lengthLeft(List(7)) should be (1)
  }

  it should "return 3 for a three-item list" in {
    List.lengthLeft(List(2,3,4)) should be (3)
  }


  behavior of "reverse"

  it should "return Nil for Nil" in {
    List.reverse(Nil) should be (Nil)
  }

  it should "return a single-item list unchanged" in {
    List.reverse(List(1)) should be (List(1))
  }

  it should "return a multi-item list reversed" in {
    List.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
  }


  behavior of "string concatenation"

  def appendRight(s: String, list: List[String]) = Cons(s, list)
  def appendLeft(list: List[String], s: String) = Cons(s, list)

  val strings = List("a", "b", "c")

  it should "be in normal order from the right" in {
    List.foldLeft(strings, Nil: List[String])(appendLeft) should be (List("c", "b", "a"))
  }

  it should "be in normals order from the right" in {
    List.foldRight(strings, Nil: List[String])(appendRight) should be (strings)
  }


  behavior of "implementing each other"

  it should "look like right, but be left" in {
    val proper = List.foldRight(strings, Nil: List[String])(appendRight)
    List.foldRightUsingLeft(strings, Nil: List[String])(appendRight) should be (proper)
  }

  it should "look like left, but be right" in {
    val proper = List.foldLeft(strings, Nil: List[String])(appendLeft)
    List.foldLeftUsingRight(strings, Nil: List[String])(appendLeft) should be (proper)
  }



  behavior of "append"

  it should "Nil + Nil = Nil" in {
    List.append[String](Nil, Nil) should be (Nil)
  }

  it should "List + Nil = List" in {
    List.append[String](strings, Nil) should be (strings)
  }

  it should "Nil + Listl = List" in {
    List.append[String](Nil, strings) should be (strings)
  }

  it should "combine two lists" in {
    List.append[String](strings, List("e", "f")) should be (List("a", "b", "c", "e", "f"))
  }


  behavior of "appendWithFold"

  it should "Nil + Nil = Nil" in {
    List.appendWithFold[String](Nil, Nil) should be (Nil)
  }

  it should "List + Nil = List" in {
    List.appendWithFold[String](strings, Nil) should be (strings)
  }

  it should "Nil + Listl = List" in {
    List.appendWithFold[String](Nil, strings) should be (strings)
  }

  it should "combine two lists" in {
    List.appendWithFold[String](strings, List("e", "f")) should be (List("a", "b", "c", "e", "f"))
  }


  behavior of "concatLists"

  it should "return Nil for Nil" in {
    List.concatLists(Nil) should be (Nil)
  }

  it should "return itself for a single list" in {
    List.concatLists(List(List(1,2,3))) should be (List(1,2,3))
  }

  it should "return combined for multiple lists" in {
    List.concatLists(List(List(1,2,3), List(7,8,9), List(0, 1, 2))) should be (List(1, 2, 3, 7, 8, 9, 0, 1, 2))
  }


  behavior of "addOne"

  it should "return Nil for Nil" in {
    List.addOne(Nil) should be (Nil)
  }

  it should "add successfully" in {
    List.addOne(List(1,2,3)) should be (List(2,3,4))
  }


  behavior of "doublesToStrings"

  it should "return Nil for Nil" in {
    List.doublesToStrings(Nil) should be (Nil)
  }

  it should "add successfully" in {
    List.doublesToStrings(List(1.0,2.0,3.0)) should be (List("1.0", "2.0", "3.0"))
  }



  behavior of "map"

  it should "return Nil for Nil" in {
    List.map(Nil)(_.toString) should be (Nil)
  }

  it should "map a list" in {
    List.map(List(1.0,2.0,3.0))(_.toString) should be (List("1.0", "2.0", "3.0"))
  }


  behavior of "filter"

  def isEven(i: Int) = i % 2 == 0

  it should "return Nil for Nil" in {
    List.filter[Int](Nil)(isEven) should be (Nil)
  }

  it should "map a list" in {
    List.filter(List(1,2,3,4,5))(isEven) should be (List(2,4))
  }


  behavior of "flatMap"

  def fm(i: Int) = List(i.toString, " ")

  it should "return Nil for Nil" in {
    List.flatMap[Int,String](Nil)(fm) should be (Nil)
  }

  it should "map a list" in {
    List.flatMap(List(1, 2, 3))(fm) should be (List("1", " ", "2", " ", "3", " "))
  }

  it should "pass the book's test" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
  }

  behavior of "flatMap2"

  it should "return Nil for Nil" in {
    List.flatMap2[Int,String](Nil)(fm) should be (Nil)
  }

  it should "map a list" in {
    List.flatMap2(List(1, 2, 3))(fm) should be (List("1", " ", "2", " ", "3", " "))
  }

  it should "pass the book's test" in {
    List.flatMap2(List(1, 2, 3))(i => List(i, i)) should be (List(1, 1, 2, 2, 3, 3))
  }


  behavior of "filterUsingFlatMap"

  it should "return Nil for Nil" in {
    List.filterUsingFlatMap[Int](Nil)(isEven) should be (Nil)
  }

  it should "map a list" in {
    List.filterUsingFlatMap(List(1,2,3,4,5))(isEven) should be (List(2,4))
  }



  behavior of "addingCorrespondingLists"

  it should "return Nil for Nil" in {
    List.addingCorrespondingLists(Nil, Nil) should be (Nil)
  }

  it should "add two lists" in {
    List.addingCorrespondingLists(List(1,2,3), List(2, 3, 4)) should be (List(3, 5, 7))
  }

  it should "add two lists the same way in different order" in {
    List.addingCorrespondingLists(List(2, 3, 4), List(1, 2, 3)) should be (List(3, 5, 7))
  }

  behavior of "hasSubsequence"

  // I'm going to do this proper TDD style, writing each test, making it work, iterate,
  //  even if the implementation is obviously not going to work for future tests

  it should "return false for Nil list" in {
    List.hasSubsequence(Nil, List(1)) should be (false)
  }

  it should "return false for both Nil" in {
    List.hasSubsequence(Nil, Nil) should be (false)
  }

  it should "return true for Nil subsequence" in {
    List.hasSubsequence(List(1), Nil) should be (true)
  }

  it should "return true for itself" in {
    List.hasSubsequence(List(1), List(1)) should be (true)
  }

  it should "return false for two different single-item lists" in {
    List.hasSubsequence(List(1), List(2)) should be (false)
  }

  it should "return false for when checked list is too small" in {
    List.hasSubsequence(List(1), List(1, 2)) should be (false)
  }

  it should "return true simplest case where sub isn't the head" in {
    List.hasSubsequence(List(1, 2), List(2)) should be (true)
  }

  it should "return true for same lists, with more than 1 item" in {
    List.hasSubsequence(List(1, 2), List(1, 2)) should be (true)
  }

  it should "return false when heads are same. but rest isn't" in {
    List.hasSubsequence(List(1, 2), List(1, 3)) should be (false)
  }

  it should "return true for multi-item candidate in middle" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be (true)
  }

  it should "return true for multi-item candidate at the end" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(3, 4)) should be (true)
  }

  // then once all those tests passed, I refactored to stream-line things to be more like
  //  I would have written in the first place

  behavior of "zipWith"

  def zipF(i: Int, d: Double) = (i*d).toString
  def zipF2(d: Double, i: Int) = (i*d).toString

  it should "return Nil for Nil" in {
    List.zipWith(Nil, Nil, zipF) should be (Nil)
  }

  it should "add two lists" in {
    List.zipWith(List(1,2,3), List(2.0, 3.0, 4.0), zipF) should be (List("2.0", "6.0", "12.0"))
  }

  it should "add two lists the same way in different order" in {
    List.zipWith(List(2.0, 3.0, 4.0), List(1,2,3), zipF2) should be (List("2.0", "6.0", "12.0"))
  }
}


