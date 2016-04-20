package fpinscala.streamingio

import org.scalatest._
import SimpleStreamTransducers._

class ProcessSpec extends FlatSpec with Matchers {


  // 15.1

  behavior of "take"

  it should "take nothing on zero" in {
    Process.take(0)(Stream(1, 2, 3)).toList should be (List())
  }

  it should "take 2 on 2" in {
    Process.take(2)(Stream(1, 2, 3)).toList should be (List(1, 2))
  }


  behavior of "drop"

  it should "return entire stream for zero" in {
    Process.drop(0)(Stream(1, 2, 3)).toList should be (List(1, 2, 3))
  }

  it should "take leave only the 3rd on 2" in {
    Process.drop(2)(Stream(1, 2, 3)).toList should be (List(3))
  }


  behavior of "takeWhile"

  it should "take nothing when fails right off" in {
    Process.takeWhile[Int](_ < 0)(Stream(1, 2, 3)).toList should be (List())
  }

  it should "take some" in {
    Process.takeWhile[Int](_ < 0)(Stream(-1, -2, 3, -4)).toList should be (List(-1, -2))
  }


  behavior of "dropWhile"

  it should "drop nothing if always false" in {
    Process.dropWhile[Int](_ < 0)(Stream(1, 2, 3)).toList should be (List(1, 2, 3))
  }

  it should "drop the first couple" in {
    Process.dropWhile[Int](_ < 0)(Stream(-1, -2, 3, -4)).toList should be (List(3, -4))
  }


  // 15.2


  behavior of "count2"

  it should "return empty for empty stream" in {
    Process.count2(Stream()).toList should be (List())
  }

  it should "return accumulated counts" in {
    Process.count2(Stream('f', 'o', 'o', 'b')).toList should be (List(1,2,3,4))
  }


  behavior of "count"

  it should "return empty for empty stream" in {
    Process.count(Stream()).toList should be (List())
  }

  it should "return accumulated counts" in {
    Process.count(Stream('f', 'o', 'o', 'b')).toList should be (List(1,2,3,4))
  }

}

