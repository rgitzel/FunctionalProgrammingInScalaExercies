package fpinscala.monads

import fpinscala.monads.Reader._
import org.scalatest._


class ReaderSpec extends FlatSpec with Matchers {

  // 11.20


  behavior of "Reader"

  val stringToIntReader     = Reader( (r: String) => r.toInt )
  val stringToDoubleReader  = Reader( (r: String) => r.toDouble )

  // for sake of argument we'll ignore parsing errors

  it should "convert from string to int" in {
    stringToIntReader.run("4") should be (4)
  }

  it should "convert from string to double" in {
    stringToIntReader.run("4") should be (4.0)
  }


  behavior of "flatMap"

  it should "return double of given int" in {
    def multiplier(a: Int): Reader[String,Double] = Reader( (r: String) => a * r.toDouble)

    val mapped: Reader[String,Double] = readerMonad.flatMap(stringToIntReader)(multiplier)

    mapped.run("4") should be (4 * 4.0)
  }

  // that doesn't make much sense... and I don't quite buy the answer... they handwave over
  //  an actual example :(
}
