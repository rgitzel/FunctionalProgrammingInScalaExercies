package fpinscala.exceptions

import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {

   // 4.6

   behavior of "map"

   def mapF(i: Int) = i.toString

   it should "map Left to Left" in {
     Left("x").map(mapF) should be (Left("x"))
   }

   it should "map Right to Right" in {
     Right(17).map(mapF) should be (Right("17"))
   }


   behavior of "flatMap"

   def flatMapF(i: Int) = if(i < 0) Left("negative") else Right(i.toString)

   it should "map Left to Left" in {
     Left("x").flatMap(flatMapF) should be (Left("x"))
   }

   it should "map Right to Right" in {
     Right(17).flatMap(flatMapF) should be (Right("17"))
   }

   it should "map Right to Left" in {
     Right(-1).flatMap(flatMapF) should be (Left("negative"))
   }


   behavior of "orElse"

   it should "return the Right else from a Left" in {
     Left("x").orElse(Right(27)) should be (Right(27))
   }

   it should "return the Left else from a Left" in {
     Left("x").orElse(Left("y")) should be (Left("y"))
   }

   it should "return the provided Right" in {
     Right(17).orElse(Left("x")) should be (Right(17))
   }


   behavior of "map2"

   def map2f(a: Int, b: Boolean) = a.toString + b.toString

   it should "return first Left for Left, Left" in {
     Left("x").map2(Left("y"))(map2f) should be (Left("x"))
   }

  it should "return Left for Left, Right" in {
    Left("x").map2(Right(false))(map2f) should be (Left("x"))
  }

  it should "return Left for Right, Left" in {
    Right(1).map2(Left("y"))(map2f) should be (Left("y"))
  }

  it should "return Right for Right, Right" in {
    Right(1).map2(Right(false))(map2f) should be (Right("1false"))
  }


   // 4.7

   behavior of "sequence"

   it should "return Right for empty list" in {
     Either.sequence(List()) should be (Right(List()))
   }

   it should "return Left for list of one Left" in {
     Either.sequence(List(Left("x"))) should be (Left("x"))
   }

   it should "return Right for list of one Right" in {
     Either.sequence(List(Right(1))) should be (Right(List(1)))
   }

   it should "return Right for list of two Rights" in {
     Either.sequence(List(Right(1), Right(2))) should be (Right(List(1, 2)))
   }

   it should "return Left for list starting with Left" in {
     Either.sequence(List(Left("x"), Right(1), Right(2))) should be (Left("x"))
   }

   it should "return Left for list ending with Left" in {
     Either.sequence(List(Right(1), Right(2), Left("x"))) should be (Left("x"))
   }

   it should "return Left for list with Left in the middle" in {
     Either.sequence(List(Right(1), Left("x"), Right(2))) should be (Left("x"))
   }

  it should "return the first Left for list with multiple Lefts" in {
    Either.sequence(List(Right(1), Left("v"), Left("x"))) should be (Left("v"))
  }


//   behavior of "traverse"
// this is getting tedious... I'm not going to do all the cases here.  sequence()
//  is implemented with traverse() so if the above tests pass that's good enough for this exercise
 }


