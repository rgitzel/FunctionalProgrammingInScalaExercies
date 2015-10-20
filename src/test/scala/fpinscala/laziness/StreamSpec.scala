package fpinscala.laziness

import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

	// 5.1

	behavior of "toList"

	it should "Empty should be Nil" in {
		Empty.toList should be (Nil)
	}

	it should "convert head with no tail to single-item list" in {
		Stream(1).toList should be(List(1))
	}	

	it should "convert head with tail to multi-item list" in {
		Stream(1, 2, 3).toList should be(List(1, 2, 3))
	}	


	// 5.2

	behavior of "take"

	it should "return nothing for empty stream" in {
		Empty.take(1) should be (Empty)
	}

	it should "return nothing for n=0" in {
		Stream(1,2,3).take(0) should be (Empty)
	}

	it should "return one item for n=1" in {
		Stream(1,2,3).take(1).toList should be (List(1))
	}

	it should "return all items for n=4" in {
		Stream(1,2,3).take(4).toList should be (List(1, 2, 3))
	}


	behavior of "drop"

	it should "return nothing for empty stream" in {
		Empty.drop(1) should be (Empty)
	}

	it should "return entire stream for n=0" in {
		Stream(1,2,3).drop(0).toList should be (List(1,2,3))
	}

	it should "return skip first one for n=1" in {
		Stream(1,2,3).drop(1).toList should be (List(2,3))
	}

	it should "return skip all for n=3" in {
		Stream(1,2,3).drop(3).toList should be (Nil)
	}

	it should "return skip all for n=10" in {
		Stream(1,2,3).drop(10).toList should be (Nil)
	}


	// 5.3

	behavior of "takeWhile"

	it should "return nothing for empty stream" in {
		Stream[Int]().takeWhile(_ < 10) should be (Empty)
	}

	it should "return nothing if first element false but rest are true" in {
		Stream(1,2,3).takeWhile(_ > 1) should be (Empty)
	}

	it should "return everything if all true elements" in {
		Stream(1,2,3).takeWhile(_ > 0).toList should be (List(1,2,3))
	}

	it should "return only true elements" in {
		Stream(1,2,3).takeWhile(_ < 3).toList should be (List(1, 2))
	}

	
	// 5.4

	behavior of "forAll"

	it should "return true for empty stream" in {
		Stream[Int]().forAll(_ < 10) should be (true)
	}

	it should "return false if first element false but rest are true" in {
		Stream(1,2,3).forAll(_ > 1) should be (false)
	}

	it should "return true if all true elements" in {
		Stream(1,2,3).forAll(_ > 0) should be (true)
	}


	// 5.5

	behavior of "takeWhile2"

	it should "return nothing for empty stream" in {
		Stream[Int]().takeWhile2(_ < 10).toList should be (Nil)
	}

	it should "return nothing if first element false but rest are true" in {
		Stream(1,2,3).takeWhile2(_ > 1).toList should be (Nil)
	}

	it should "return everything if all true elements" in {
		Stream(1,2,3).takeWhile2(_ > 0).toList should be (List(1,2,3))
	}

	it should "return only true elements" in {
		Stream(1,2,3).takeWhile2(_ < 3).toList should be (List(1, 2))
	}


	// 5.6

	behavior of "headOption"

	it should "return None empty stream" in {
		Stream[Int]().headOption should be (None)
	}

	it should "return first element of single-item list" in {
		Stream(1).headOption should be (Some(1))
	}


	// 5.7

	// I copied these these tests from the the List tests of chapter 3

	behavior of "map"

	def mapF(i: Int) = i.toString

	it should "return Nil for Nil" in {
		Stream.empty.map(mapF).toList should be (Nil)
	}

	it should "map a list" in {
		Stream(1, 2, 3).map(mapF).toList should be (List("1", "2", "3"))
	}


	behavior of "filter"

	def isEven(i: Int) = i % 2 == 0

	it should "return Empty for Empty" in {
		Stream.empty[Int].filter(isEven).toList should be (Nil)
	}

	it should "filter a list" in {
		Stream(1,2,3,4,5).filter(isEven).toList should be (List(2,4))
	}




	behavior of "append"

	val strings = Stream("a", "b", "c")

	it should "Nil + Nil = Nil" in {
		Stream.empty.append(Stream.empty).toList should be (Nil)
	}

	it should "List + Nil = List" in {
		strings.append(Stream.empty).toList should be (strings.toList)
	}

	it should "Nil + List = List" in {
		Stream.empty.append(strings).toList should be (strings.toList)
	}

	it should "combine two lists" in {
		strings.append(Stream("e", "f")).toList should be (List("a", "b", "c", "e", "f"))
	}


	behavior of "flatMap"

	def fm(i: Int) = Stream(i.toString, " ")

	it should "return empty for empty" in {
		Stream.empty[Int].flatMap(fm).toList should be (Nil)
	}

	it should "map a stream" in {
		Stream(1, 2, 3).flatMap(fm).toList should be (List("1", " ", "2", " ", "3", " "))
	}

	it should "pass the book's test" in {
		Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList should be (List(1, 1, 2, 2, 3, 3))
	}


	// 5.8

	behavior of "constant"

	it should "return 5 5's" in {
		Stream.constant(5).take(5).toList should be (List(5, 5, 5, 5, 5))
	}


	// 5.9

	behavior of "from"

	it should "return 5 6 7" in {
		Stream.from(5).take(3).toList should be (List(5, 6, 7))
	}



	// 5.10

	behavior of "fibs"

	val fibsTo10 = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

	it should "return 0, 1 first" in {
		Stream.fibs.take(2).toList should be (List(0, 1))
	}


	it should "return the usual sequence" in {
		Stream.fibs.take(10).toList should be (fibsTo10)
	}


	// 5.12

	behavior of "fibs2"

	it should "return the usual sequence" in {
		Stream.fibs2.take(10).toList should be (fibsTo10)
	}


	behavior of "from2"

	it should "return 5 6 7" in {
		Stream.from2(5).take(3).toList should be (List(5, 6, 7))
	}


	behavior of "constant2"

	it should "return 5 5's" in {
		Stream.constant2(5).take(5).toList should be (List(5, 5, 5, 5, 5))
	}


	behavior of "ones2"

	it should "return 5 1's" in {
		Stream.ones2.take(5).toList should be (List(1, 1, 1, 1, 1))
	}

	// 5.13

	behavior of "map2"

	it should "return Nil for Nil" in {
		Stream.empty.map2(mapF).toList should be (Nil)
	}

	it should "map a list" in {
		Stream(1, 2, 3).map2(mapF).toList should be (List("1", "2", "3"))
	}


	behavior of "take2"

	it should "return nothing for empty stream" in {
		Empty.take2(1) should be (Empty)
	}

	it should "return nothing for n=0" in {
		Stream(1,2,3).take2(0) should be (Empty)
	}

	it should "return one item for n=1" in {
		Stream(1,2,3).take2(1).toList should be (List(1))
	}

	it should "return all items for n=4" in {
		Stream(1,2,3).take2(4).toList should be (List(1, 2, 3))
	}


	behavior of "takeWhile3"

	it should "return nothing for empty stream" in {
		Stream[Int]().takeWhile3(_ < 10).toList should be (Nil)
	}

	it should "return nothing if first element false but rest are true" in {
		Stream(1,2,3).takeWhile3(_ > 1).toList should be (Nil)
	}

	it should "return everything if all true elements" in {
		Stream(1,2,3).takeWhile3(_ > 0).toList should be (List(1,2,3))
	}

	it should "return only true elements" in {
		Stream(1,2,3).takeWhile3(_ < 3).toList should be (List(1, 2))
	}

}
