package fpinscala.localeffects

import org.scalatest._


class STArraySpec extends FlatSpec with Matchers {


  behavior of "tinkering"

  it should "double the values" in {
    val list = List(1,2,3,4)

    val x = ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        array <- STArray.fromList(list)
        size <- array.size
// I thought this would work
//        _ <- ( for {
//                    i <- 0 until size
//                    x <- array.read(i)
//                    _ <- array.write(i, 2 * x)
//                } yield ST(())
//            )

        // yikes! kinda stole this from the answer for 'partition'...
        _ <- (0 until size).foldLeft(ST[S,Unit](())) { (s, i) =>
          for {
            _ <- s  // critical... why?... without this, we get (1, 2, 3, 8)
            x <- array.read(i)
            _ <- array.write(i, 2 * x)
          } yield ()
        }

        result <- array.freeze
      } yield result
    })

    x should be (List(2, 4, 6, 8))
  }


  // 14.1

  behavior of "fill"

  def testFill(m: Map[Int, String], expected: List[String]): Unit = {
    val x = ST.runST(new RunnableST[List[String]] {
      def apply[S] = for {
        array <- STArray(expected.size, "")
        _ <- array.fill(m)
        result <- array.freeze
      } yield result
    })
    x should be (expected)
  }


  it should "work on 2 items" in {
    val m = Map(
      0 -> "a",
      2 -> "b"
    )

    val expected = List("a", "", "b")

    testFill(m, expected)
  }


  it should "work on 5 items" in {
    val m = Map(
      5 -> "y",
      0 -> "a",
      3 -> "foo",
      2 -> "b",
      9 -> "bar"
    )

    val expected = List("a", "", "b", "foo", "", "y", "", "", "", "bar")

    testFill(m, expected)
  }


  // 11.2

  behavior of "partition"

  // holy cow, seriously?  have you looked at the answer? you have to really really believe in this technique :(




}
