package fpinscala.parallelism

import java.util.concurrent.{TimeUnit, Executors}

import org.scalatest._
import Par._

class ParSpec extends FlatSpec with Matchers {

  // interesting... WorkStealingPool doesn't deadlock, but only steals 3-4 threads?
  //  parWordCount would block waiting for threads with only 5 input strings
  //val es = Executors.newWorkStealingPool()
  val es = Executors.newFixedThreadPool(100)

  def equate[A](a: Par[A], b: Par[A]) = a(es).get should be (b(es).get)

  // NOTE!!!  if a, b aren't lazy, elapsed is always <= 1
  def equateWithin[A](millis: Int, a: => Par[A], b: => Par[A]) = {
    val start = System.currentTimeMillis
    val expected = b(es).get
    val returned = a(es).get
    returned should be (expected)
    val elapsed = (System.currentTimeMillis - start).toInt
    println(s"${returned}, ${expected} => ${elapsed}")
    elapsed should be < millis
  }



  // 7.5

  behavior of "ParSpec"

  it should "return Par[Nil] for Nil" in {
    equate(sequence(List[Par[Int]]()), unit(List[Int]()))
  }

  it should "return a list for a list" in {
    equate(sequence(List(unit(1), unit(2), unit(3))), unit(List(1, 2, 3)))
  }



  // 7.6

  behavior of "ParFilter"

  def filterF(n: Int) = n % 2 == 0

  it should "return Par[Nil] for Nil" in {
    val list = List[Int]()
    val expected = unit(List[Int]())
    equate(parFilter(list)(filterF), expected)
  }

  it should "return a list for a list" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val expected = unit(List[Int](2, 4))
    equate(parFilter(list)(filterF), expected)
  }


  // extra questions

  behavior of "parSum"

  it should "return 0 for Nil" in {
    equate(parSum2(IndexedSeq()), unit(0))
  }

  it should "return value for single-item list" in {
    equate(parSum2(IndexedSeq(7)), unit(7))
  }

  it should "return sum for multi-item list" in {
    equate(parSum2(IndexedSeq(7, 12, -7, 1, -13, 2)), unit(2))
  }


  behavior of "parMax"

  it should "return MinValue for Nil" in {
    equate(parMax(IndexedSeq()), unit(Int.MinValue))
  }

  it should "return value for single-item list" in {
    equate(parMax(IndexedSeq(7)), unit(7))
  }

  it should "return sum for multi-item list" in {
    equate(parMax(IndexedSeq(7, 12, -7, -100, 4, 8)), unit(12))
  }

  it should "return sum for multi-item list where max is last" in {
    equate(parMax(IndexedSeq(7, 12, -7, -100, 4, 8, 99)), unit(99))
  }


  behavior of "parConcat"

  it should "return empty string for Nil" in {
    equate(parConcat(IndexedSeq()), unit(""))
  }

  it should "return value for single-item list" in {
    equate(parConcat(IndexedSeq(7)), unit("7"))
  }

  it should "return full string for multi-item list" in {
    equate(parConcat(IndexedSeq(7, 12, -7)), unit("712-7"))
  }


  behavior of "parWordCountWithIndexedSeq"

  it should "return 0 for Nil" in {
    equate(parWordCountWithIndexedSeq(100, IndexedSeq()), unit(0))
  }

  it should "return 1 for single-item single-word list" in {
    equate(parWordCountWithIndexedSeq(100, IndexedSeq("foo")), unit(1))
  }

  it should "return 3 for single-item three-word list" in {
    equate(parWordCountWithIndexedSeq(100, IndexedSeq("foo bar baz")), unit(3))
  }

  it should "return proper count for multi-item list and do it all in parallel" in {
    val list = IndexedSeq(
      "1foo bar baz",
      "2monkeys in a tree",
      "the quick brown fox be jumping",
      "3foo",
      "4whoa nellie",
      "5a b c"
    )
    val delay = 200
    equateWithin[Int](
      (delay * 1.2).toInt,
      parWordCountWithIndexedSeq(delay, list ++ list ++ list),
      unit(3 * 19)
    )
  }
}

