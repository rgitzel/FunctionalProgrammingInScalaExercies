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


	behavior of "sequence"

	it should "can combine 0 functions" in {
		val f = State.sequence[RNG,Int](Nil)
		val (a, next) = f.run(FixedRNG(List(17, 5, 44)))
		a should be(Nil)
		next.nextInt._1 should be(17)
	}

	it should "can combine 1 function" in {
		val f = State.sequence(List(State(RNG.nonNegativeEven)))
		val (a, next) = f.run(FixedRNG(List(17, 5, 44)))
		a should be(List(16))
		next.nextInt._1 should be(5)
	}


	it should "can combine 3 functions" in {
		// 'nonNegativeInt' isn't declared as a Rand[A], but as a def, so it's not as accessible as
		//  a "function" declared explicitly?
		val f = State.sequence(List(State(RNG.nonNegativeEven), State(RNG.nonNegativeInt _), State(RNG.nonNegativeEven)))
		val (a, next) = f.run(FixedRNG(List(17, 5, 43, 12)))
		a should be(List(16, 5, 42))
		next.nextInt._1 should be(12)
	}

}
