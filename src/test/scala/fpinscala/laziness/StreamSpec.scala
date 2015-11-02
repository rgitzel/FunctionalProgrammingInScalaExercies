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


	behavior of "zipWith"

	def zipF(i: Int, d: Double) = (i*d).toString
	def zipF2(d: Double, i: Int) = (i*d).toString

	it should "return Nil for Nil" in {
		Stream.empty.zipWith(Stream(10.0, 20.0, 30.0), zipF).toList should be (Nil)
	}

	it should "add two lists" in {
		Stream(1, 2, 3).zipWith(Stream(10.0, 20.0, 30.0), zipF).toList should be (List("10.0", "40.0", "90.0"))
	}

	it should "add two lists the same way in different order" in {
		Stream(10.0, 20.0, 30.0).zipWith(Stream(1, 2, 3), zipF2).toList should be (List("10.0", "40.0", "90.0"))
	}


	behavior of "zipAll"

	it should "return empty when both lists empty" in {
		Stream.empty.zipAll(Stream.empty).toList should be (Nil)
	}

	it should "combine when first list is empty" in {
		Stream.empty.zipAll(Stream(1, 2)).toList should be (
			List(
				(None, Some(1)),
				(None, Some(2))
			)
		)
	}

	it should "combine when second list is empty" in {
		Stream(1, 2).zipAll(Stream.empty).toList should be (
			List(
				(Some(1), None),
				(Some(2), None)
			)
		)
	}

	it should "combine two finite lists" in {
		Stream(1, 2).zipAll(Stream(7, 8, 9)).toList should be (
			List(
				(Some(1), Some(7)),
				(Some(2), Some(8)),
				(None,    Some(9))
			)
		)
	}

	it should "combine two infinite lists" in {
		Stream.from(1).zipAll(Stream.from(2)).take(4).toList should be (
			List(
				(Some(1), Some(2)),
				(Some(2), Some(3)),
				(Some(3), Some(4)),
				(Some(4), Some(5))
			)
		)
	}

	// 5.14

	behavior of "startsWith"

	it should "work on itself" in {
		Stream(1).startsWith(Stream(1)) should be (true)
	}

	it should "be false on not itself" in {
		Stream(1).startsWith(Stream(2)) should be (false)
	}

	it should "be true for book example" in {
		Stream(1, 2, 3).startsWith(Stream(1, 2)) should be (true)
	}

	it should "be false if substring is longer" in {
		Stream(1, 2).startsWith(Stream(1, 2, 3)) should be (false)
	}

	it should "be true when sub matches infinite stream " in {
		println()
		Stream.from(1).startsWith(Stream(1, 2, 3)) should be (true)
	}

	it should "be false when sub doesn't match infinite stream" in {
		println()
		Stream.from(1).startsWith(Stream(1, 2, 4)) should be (false)
	}



	// 5.15

	behavior of "tails"

	it should "work for book example" in {
		Stream(1, 2, 3).tails.toList.map(_.toList) should be(
		  List(
			  List(1, 2, 3),
			  List(2, 3),
			  List(3),
			  List()
			)
		)
	}


	// 5.16

	behavior of "scanRightUsingFoldRight"

	it should "work for book example" in {
		Stream(1, 2, 3).scanRightUsingFoldRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
	}

	it should "work for string concatenation" in {
		Stream("1", "2", "3").scanRightUsingFoldRight("")(_ + _).toList should be(List("123", "23", "3", ""))
	}


	behavior of "scanRightUsingUnfold"

	it should "not work for book example" in {
		val s = Stream(1, 2, 3).scanRightUsingUnfold(0)(_ + _).toList

		s should not be(List(6, 5, 3, 0))

		// unfold does it in the wrong order
		s should be(List(0, 1, 3, 6))
	}

}
