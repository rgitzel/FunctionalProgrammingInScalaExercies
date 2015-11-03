package fpinscala.state

import org.scalatest._

class RngSpec extends FlatSpec with Matchers {


	behavior of "nextInt"

	it should "should return same random for same seed" in {
		RNG.Simple(1847).nextInt._1 should be(RNG.Simple(1847).nextInt._1)
	}

	// 6.1

	behavior of "nonNegativeValue"

	def nonNegTest(in: Int, out: Int): Unit = {
		RNG.nonNegativeInt(FixedRNG(List(in)))._1 should be(out)
	}

	it should "can return 0" in {
		nonNegTest(-1, 0)
	}

	it should "can return 1" in {
		nonNegTest(1, 1)
	}

	it should "-1 should return positive" in {
		nonNegTest(-1, 0)
	}

	it should "big positive value should return positive value" in {
		nonNegTest(23452346, 23452346)
	}

	it should "big negative value should return positive value" in {
		nonNegTest(-23452346, 23452345)
	}

	// it's not necessarily an even distribution, but we've shown we can generate the desired range

	it should "minValue should return maxVAlue" in {
		nonNegTest(Int.MinValue, Int.MaxValue)
	}


	// 6.2

	behavior of "double"

	it should "can return 0" in {
		val d = RNG.double(FixedRNG(List(0, 1)))
		d._1 should be(0.0)
		d._2.nextInt._1 should be(1)
	}

	it should "return close to 1.0 but not 1.0" in {
		val d = RNG.double(FixedRNG(List(Int.MaxValue)))._1
		d should be < 1.0
		(1.0 - d) should be < 0.000000001
	}


	// 6.4

	behavior of "ints"

	val intsRng = FixedRNG(List(45, 35, 6, 1, 55))

	it should "return empty list for 0" in {
		val i = RNG.ints(0)(intsRng)
		i._1 should be(Nil)
		i._2.nextInt._1 should be(45)
	}

	it should "return 1-item list for 1" in {
		val i = RNG.ints(1)(intsRng)
		i._1 should be(List(45))
		i._2.nextInt._1 should be(35)
	}

	it should "return multiple ints" in {
		val i = RNG.ints(4)(intsRng)
		i._1 should be(List(45, 35, 6, 1))
		i._2.nextInt._1 should be(55)
	}


	// 6.5

	behavior of "doubleViaMap"

	it should "can return 0" in {
		val d = RNG.doubleViaMap(FixedRNG(List(0, 1)))
		d._1 should be(0.0)
		d._2.nextInt._1 should be(1)
	}

	it should "return close to 1.0 but not 1.0" in {
		val d = RNG.doubleViaMap(FixedRNG(List(Int.MaxValue)))._1
		d should be < 1.0
		(1.0 - d) should be < 0.000000001
	}


	// 6.6

	behavior of "map2"

	it should "can combine two functions" in {
		val f = RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)((_, _))
		val result = f(FixedRNG(List(17, 5, 44)))
		result._1 should be((17, 5))
		result._2.nextInt._1 should be(44)
	}


	// 6.7

	behavior of "sequence"

	it should "can combine 0 functions" in {
		val f = RNG.sequence(Nil)
		val result = f(FixedRNG(List(17, 5, 44)))
		result._1 should be(Nil)
		result._2.nextInt._1 should be(17)
	}

	it should "can combine 1 function" in {
		val f = RNG.sequence(List(RNG.nonNegativeInt _))
		val result = f(FixedRNG(List(17, 5, 44)))
		result._1 should be(List(17))
		result._2.nextInt._1 should be(5)
	}


	it should "can combine 3 functions" in {
		// 'nonNegativeInt' isn't declared as a Rand[A], but as a def, so it's not as accessible as
		//  a "function" declared explicitly?
		val f = RNG.sequence(List(RNG.nonNegativeEven, RNG.nonNegativeInt _, RNG.nonNegativeEven))
		val result = f(FixedRNG(List(17, 5, 43, 12)))
		result._1 should be(List(16, 5, 42))
		result._2.nextInt._1 should be(12)
	}


	behavior of "intsViaSequence"

	it should "return empty list for 0" in {
		val i = RNG.intsViaSequence(0)(intsRng)
		i._1 should be(Nil)
		i._2.nextInt._1 should be(45)
	}

	it should "return 1-item list for 1" in {
		val i = RNG.intsViaSequence(1)(intsRng)
		i._1 should be(List(45))
		i._2.nextInt._1 should be(35)
	}

	it should "return multiple ints" in {
		val i = RNG.intsViaSequence(4)(intsRng)
		i._1 should be(List(45, 35, 6, 1))
		i._2.nextInt._1 should be(55)
	}



	behavior of "sequenceViaFold"

	it should "can combine 0 functions" in {
		val f = RNG.sequenceViaFold(Nil)
		val result = f(FixedRNG(List(17, 5, 44)))
		result._1 should be(Nil)
		result._2.nextInt._1 should be(17)
	}

	it should "can combine 1 function" in {
		val f = RNG.sequenceViaFold(List(RNG.nonNegativeInt _))
		val result = f(FixedRNG(List(17, 5, 44)))
		result._1 should be(List(17))
		result._2.nextInt._1 should be(5)
	}


	it should "can combine 3 functions" in {
		// 'nonNegativeInt' isn't declared as a Rand[A], but as a def, so it's not as accessible as
		//  a "function" declared explicitly?
		val f = RNG.sequenceViaFold(List(RNG.nonNegativeEven, RNG.nonNegativeInt _, RNG.nonNegativeEven))
		val result = f(FixedRNG(List(17, 5, 43, 12)))
		result._1 should be(List(16, 5, 42))
		result._2.nextInt._1 should be(12)
	}


	// 6.8

	behavior of "flatMap"

	it should "do something" in {
		val f = RNG.flatMap(RNG.nonNegativeEven){i => RNG.unit(i.toString)}
		val (b, rng2) = f(FixedRNG(List(13,14)))
		b should be ("12")
		rng2.nextInt._1 should be (14)
	}


	behavior of "nonNegativeLessThan"

	it should "work" in {
	  val (a, rng2) = RNG.nonNegativeLessThan(5)(FixedRNG(List(Int.MaxValue - 1, 6, 13)))
		// first one should have failed, second succeeded
		a should be (6%5)
		// should have consumed two RNGs
		rng2.nextInt._1 should be (13)
  }

	// 6.9

	behavior of "mapWithFlatMap"

	it should "work" in {
		val f = RNG.mapWithFlatMap(RNG.nonNegativeEven){ i => i.toString }
	}

	behavior of "map2WithFlatMap"

	it should "can combine two functions" in {
		val f = RNG.map2WithFlatMap(RNG.nonNegativeInt, RNG.nonNegativeInt)((_, _))
		val result = f(FixedRNG(List(17, 5, 44)))
		result._1 should be((17, 5))
		result._2.nextInt._1 should be(44)
	}

}
