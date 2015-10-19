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
}
