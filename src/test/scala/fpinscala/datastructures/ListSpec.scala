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

}
