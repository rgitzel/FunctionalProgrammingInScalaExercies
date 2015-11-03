package fpinscala.state

/*
 * rather than 'generating' random numbers, this just returns what you put in.
 *  which is simpler for testing RNG code, as you don't have to figure out how
 *  to generate a specific random number
 *
 * don't call this any more times than you provide values for or you will get
 *  an exception for calling head on an empty list
 */

case class FixedRNG(nexts: List[Int]) extends RNG {
  def nextInt = (nexts.head, FixedRNG(nexts.tail))
}

