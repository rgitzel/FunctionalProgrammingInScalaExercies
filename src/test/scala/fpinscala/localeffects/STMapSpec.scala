package fpinscala.localeffects

import org.scalatest._


class STMapSpec extends FlatSpec with Matchers {


  // 14.3

  behavior of "fromMap"

  def testFromMaps[A,B](m: Map[A,B]): Unit = {
    val x = ST.runST(new RunnableST[Map[A,B]] {
      def apply[S] = for {
        hm <- STMap.fromMap(m)
        result <- hm.freeze
      } yield result
    })
    x should be (m)
  }

  it should "return empty for empty" in {
    val m = Map[Int,String](
    )

    testFromMaps(m)
  }

  it should "return one item for one item" in {
    val m = Map[Int,String](
      1 -> "a"
    )

    testFromMaps(m)
  }

  it should "return three for three" in {
    val m = Map[Int,String](
      1 -> "a",
      2 -> "b",
      3 -> "c"
    )

    testFromMaps(m)
  }


  behavior of "write"

  def testWrite[A,B](m: Map[A,B], a: A, b: B, expected: Map[A,B]): Unit = {
    val x = ST.runST(new RunnableST[Map[A,B]] {
      def apply[S] = for {
        hm <- STMap.fromMap(m)
        _ <- hm.write(a, b)
        result <- hm.freeze
      } yield result
    })
    x should be (expected)
  }

  it should "return set new value" in {
    val m = Map[Int,String](
    )

    val expected = Map(
      1 -> "a"
    )
    testWrite(m, 1, "a", expected)
  }

  it should "update existing value" in {
    val m = Map[Int,String](
      1 -> "x"
    )

    val expected = Map(
      1 -> "a"
    )
    testWrite(m, 1, "a", expected)
  }
}
