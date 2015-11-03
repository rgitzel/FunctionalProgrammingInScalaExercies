package fpinscala.state

import org.scalatest._

class StateSpec extends FlatSpec with Matchers {


	behavior of "map"

	it should "work" in {
		val r = State(RNG.nonNegativeInt)
		val s = r.map{ i => i.toString }
		val (a, next) = s.run(FixedRNG(List(12, 13)))
		a should be ("12")
		next.nextInt._1 should be (13)
	}



	behavior of "map2"

	it should "can combine two functions" in {
		val s1 = State(RNG.nonNegativeEven)
		val s2 = State(RNG.nonNegativeInt)
		val (a, next) = s1.map2(s2)(_ + _).run(FixedRNG(List(17, 5, 44)))
		a should be(21)
		next.nextInt._1 should be(44)
	}


	behavior of "flatMap"

	it should "do something" in {
		val r = State(RNG.nonNegativeInt)
		val s = r.flatMap{ i => State.unit(i.toString) }
		val (a, next) = s.run(FixedRNG(List(12, 13)))
		a should be ("12")
		next.nextInt._1 should be (13)
	}

	//	// 6.7
//
//	behavior of "sequence"
//
//	it should "can combine 0 functions" in {
//		val f = RNG.sequence(Nil)
//		val result = f(FixedRNG(List(17, 5, 44)))
//		result._1 should be(Nil)
//		result._2.nextInt._1 should be(17)
//	}
//
//	it should "can combine 1 function" in {
//		val f = RNG.sequence(List(RNG.nonNegativeInt _))
//		val result = f(FixedRNG(List(17, 5, 44)))
//		result._1 should be(List(17))
//		result._2.nextInt._1 should be(5)
//	}
//
//
//	it should "can combine 3 functions" in {
//		// 'nonNegativeInt' isn't declared as a Rand[A], but as a def, so it's not as accessible as
//		//  a "function" declared explicitly?
//		val f = RNG.sequence(List(RNG.nonNegativeEven, RNG.nonNegativeInt _, RNG.nonNegativeEven))
//		val result = f(FixedRNG(List(17, 5, 43, 12)))
//		result._1 should be(List(16, 5, 42))
//		result._2.nextInt._1 should be(12)
//	}
//
//
//	behavior of "intsViaSequence"
//
//	it should "return empty list for 0" in {
//		val i = RNG.intsViaSequence(0)(intsRng)
//		i._1 should be(Nil)
//		i._2.nextInt._1 should be(45)
//	}
//
//	it should "return 1-item list for 1" in {
//		val i = RNG.intsViaSequence(1)(intsRng)
//		i._1 should be(List(45))
//		i._2.nextInt._1 should be(35)
//	}
//
//	it should "return multiple ints" in {
//		val i = RNG.intsViaSequence(4)(intsRng)
//		i._1 should be(List(45, 35, 6, 1))
//		i._2.nextInt._1 should be(55)
//	}
//
//
//
//	behavior of "sequenceViaFold"
//
//	it should "can combine 0 functions" in {
//		val f = RNG.sequenceViaFold(Nil)
//		val result = f(FixedRNG(List(17, 5, 44)))
//		result._1 should be(Nil)
//		result._2.nextInt._1 should be(17)
//	}
//
//	it should "can combine 1 function" in {
//		val f = RNG.sequenceViaFold(List(RNG.nonNegativeInt _))
//		val result = f(FixedRNG(List(17, 5, 44)))
//		result._1 should be(List(17))
//		result._2.nextInt._1 should be(5)
//	}
//
//
//	it should "can combine 3 functions" in {
//		// 'nonNegativeInt' isn't declared as a Rand[A], but as a def, so it's not as accessible as
//		//  a "function" declared explicitly?
//		val f = RNG.sequenceViaFold(List(RNG.nonNegativeEven, RNG.nonNegativeInt _, RNG.nonNegativeEven))
//		val result = f(FixedRNG(List(17, 5, 43, 12)))
//		result._1 should be(List(16, 5, 42))
//		result._2.nextInt._1 should be(12)
//	}
//
//

//
//	behavior of "nonNegativeLessThan"
//
//	it should "work" in {
//	  val (a, rng2) = RNG.nonNegativeLessThan(5)(FixedRNG(List(Int.MaxValue - 1, 6, 13)))
//		// first one should have failed, second succeeded
//		a should be (6%5)
//		// should have consumed two RNGs
//		rng2.nextInt._1 should be (13)
//  }


}
