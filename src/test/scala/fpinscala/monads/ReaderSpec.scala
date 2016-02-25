package fpinscala.monads

import fpinscala.monads.Reader._
import org.scalatest._


class ReaderSpec extends FlatSpec with Matchers {

  // 11.20

  val stringToIntReader     = Reader( (r: String) => r.toInt )
  val stringTo2xIntReader   = Reader( (r: String) => 2 * r.toInt )
  val stringTo3xIntReader   = Reader( (r: String) => 3 * r.toInt )
  val stringToDoubleReader  = Reader( (r: String) => r.toDouble )

  // for sake of argument we'll ignore parsing errors


  behavior of "Reader"

  it should "convert from string to int" in {
    stringToIntReader.run("4") should be (4)
  }

  it should "convert from string to double" in {
    stringToIntReader.run("4") should be (4.0)
  }


  behavior of "readerMonad.flatMap"

  it should "return double of given int" in {
    def multiplier(a: Int): Reader[String,Double] = Reader( (r: String) => a * r.toDouble)

    val mapped: Reader[String,Double] = readerMonad.flatMap(stringToIntReader)(multiplier)

    mapped.run("4") should be (4 * 4.0)
  }

  // that doesn't make much sense... and I don't quite buy the answer... they handwave over
  //  an actual example :(


  behavior of "readerMonad.sequence"

  it should "execute three 'functions' on same input" in {
    val list = List(stringToIntReader, stringTo2xIntReader, stringTo3xIntReader)
    readerMonad.sequence(list).run("4") should be (List(4, 8, 12))
  }


  behavior of "readerMonad.replicateM"

  it should "execute the same function multiple times on same input... uh... why???" in {
    val n = 5
    readerMonad.replicateM(n, stringTo2xIntReader).run("4") should be (List.fill(n)(2 * 4))
  }


  behavior of "readerMonad.join"

  it should "uh... just execute the inner one? who would do this?" in {
    val r: Reader[String,Reader[String,Int]] = Reader( r => stringTo2xIntReader)
    readerMonad.join(r).run("4") should be (stringTo2xIntReader.run("4"))
  }

}
