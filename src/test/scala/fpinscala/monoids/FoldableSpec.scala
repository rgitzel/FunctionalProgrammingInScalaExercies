package fpinscala.monoids

import fpinscala.monoids.Monoid._
import org.scalatest._
import fpinscala.laziness._
import fpinscala.datastructures.{Branch,Leaf}


class FoldableSpec extends FlatSpec with Matchers {


  // 10.12

  behavior of "ListFoldable foldLeft"

  it should "work on an empty list" in {
    ListFoldable.foldLeft(Nil)(""){(acc, i) => acc + i.toString} should be ("")
  }

  it should "work on a non-empty list" in {
    ListFoldable.foldLeft(List(1,2,3))(""){(acc, i) => acc + i.toString} should be ("123")
  }


  behavior of "ListFoldable foldRight"

  it should "work on an empty list" in {
    ListFoldable.foldRight(Nil)(""){(i, acc) => acc + i.toString} should be ("")
  }

  it should "work on a non-empty list" in {
    ListFoldable.foldRight(List(1,2,3))(""){(i, acc) => acc + i.toString} should be ("321")
  }


  behavior of "ListFoldable foldMap"

  it should "work on an empty list" in {
    ListFoldable.foldMap(Nil)(_.toString)(stringMonoid) should be ("")
  }

  it should "work on a non-empty list" in {
    ListFoldable.foldMap(List(4,5,6))(_.toString)(stringMonoid) should be ("456")
  }



  behavior of "IndexedSeqFoldable foldLeft"

  it should "work on an empty list" in {
    IndexedSeqFoldable.foldLeft(Vector())(""){(acc, i) => i.toString + acc} should be ("")
  }

  it should "work on a non-empty list" in {
    IndexedSeqFoldable.foldLeft(Vector(1,2,3))(""){(acc, i) => i.toString + acc} should be ("123")
  }


  behavior of "IndexedSeqFoldable foldRight"

  it should "work on an empty list" in {
    IndexedSeqFoldable.foldRight(Vector())(""){(i, acc) => acc + i.toString} should be ("")
  }

  it should "work on a non-empty list" in {
    IndexedSeqFoldable.foldRight(Vector(1,2,3))(""){(i, acc) => acc + i.toString} should be ("321")
  }


  behavior of "IndexedSeqFoldable foldMap"

  it should "work on an empty list" in {
    IndexedSeqFoldable.foldMap(Vector())(_.toString)(stringMonoid) should be ("")
  }

  it should "work on a non-empty list" in {
    IndexedSeqFoldable.foldMap(Vector(4,5,6))(_.toString)(stringMonoid) should be ("456")
  }




  behavior of "StreamFoldable foldLeft"

  it should "work on an empty list" in {
    StreamFoldable.foldLeft(Stream())(""){(acc, i) => i.toString + acc} should be ("")
  }

  it should "work on a non-empty list" in {
    StreamFoldable.foldLeft(Stream(1,2,3))(""){(acc, i) => i.toString + acc} should be ("123")
  }


  behavior of "StreamFoldable foldRight"

  it should "work on an empty list" in {
    StreamFoldable.foldRight(Stream())(""){(i, acc) => acc + i.toString} should be ("")
  }

  it should "work on a non-empty list" in {
    StreamFoldable.foldRight(Stream(1,2,3))(""){(i, acc) => acc + i.toString} should be ("321")
  }


  behavior of "StreamFoldable foldMap"

  it should "work on an empty list" in {
    StreamFoldable.foldMap(Stream())(_.toString)(stringMonoid) should be ("")
  }

  it should "work on a non-empty list" in {
    StreamFoldable.foldMap(Stream(4,5,6))(_.toString)(stringMonoid) should be ("456")
  }



  // 10.13

  val ReasonablyComplexTreeMapped =
    Branch(
      Branch(
        Leaf(2),
        Leaf(4)
      ),
      Branch(
        Branch(
          Leaf(573),
          Leaf(12)
        ),
        Leaf(9)
      )
    )


  behavior of "TreeFoldable foldMap"

  it should "work on a leaf (the simplest tree)" in {
    TreeFoldable.foldMap(Leaf(1))(_.toString)(stringMonoid) should be ("1")
  }

  it should "work on a non-trivial tree" in {
    TreeFoldable.foldMap(ReasonablyComplexTreeMapped)(_.toString)(stringMonoid) should be ("24573129")
  }

  behavior of "TreeFoldable foldLeft"

  it should "work on a leaf (the simplest tree)" in {
    TreeFoldable.foldLeft(Leaf(1))(""){(acc, i) => i.toString + acc} should be ("1")
  }

  it should "work on a non-trivial tree" in {
    TreeFoldable.foldLeft(ReasonablyComplexTreeMapped)(""){(acc, i) => i.toString + acc} should be ("24573129")
  }


  behavior of "TreeFoldable foldRight"

  it should "work on a leaf (the simplest tree)" in {
    TreeFoldable.foldRight(Leaf(1))(""){(i, acc) => acc + i.toString} should be ("1")
  }

  it should "work on a non-trivial tree" in {
    TreeFoldable.foldRight(ReasonablyComplexTreeMapped)(""){(i, acc) => acc + i.toString} should be ("91257342")
  }


  // 10.14

  behavior of "OptionFoldable foldMap"

  it should "zero on a None" in {
    OptionFoldable.foldMap(None)(_.toString)(stringMonoid) should be ("")
  }

  it should "work on a Some" in {
    OptionFoldable.foldMap(Some(9))(_.toString)(stringMonoid) should be ("9")
  }

  behavior of "OptionFoldable foldLeft"

  it should "zero on a None" in {
    OptionFoldable.foldLeft(None)("x"){(acc, i) => i.toString + acc} should be ("x")
  }

  it should "work on a Some" in {
    OptionFoldable.foldLeft(Some(8))("y"){(acc, i) => i.toString + acc} should be ("8y")
  }


  behavior of "OptionFoldable foldRight"

  it should "zero on a None" in {
    OptionFoldable.foldRight(None)("f"){(i, acc) => acc + i.toString} should be ("f")
  }

  it should "work on a Some" in {
    OptionFoldable.foldRight(Some(12))("x"){(i, acc) => acc + i.toString} should be ("x12")
  }


  // 10.15

  behavior of "toList"

  it should "work on List[A]" in {
    ListFoldable.toList(List(1,2,3)) should be (List(1,2,3))
  }

  it should "work on IndexedSeq[A]" in {
    IndexedSeqFoldable.toList(Vector(1,2,3)) should be (List(1,2,3))
  }

  it should "work on Stream[A]" in {
    StreamFoldable.toList(Stream(1,2,3)) should be (List(1,2,3))
  }

  it should "work on Tree[A]" in {
    TreeFoldable.toList(ReasonablyComplexTreeMapped) should be (List(2,4,573,12,9))
  }

  it should "work on Option[A]" in {
    OptionFoldable.toList(Some(7)) should be (List(7))
  }

}
