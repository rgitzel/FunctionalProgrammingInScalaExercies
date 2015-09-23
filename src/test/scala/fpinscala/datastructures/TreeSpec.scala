package fpinscala.datastructures

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {


  val LeafOnly = Leaf(0)

  val SimpleBranch = Branch(Leaf(0), Leaf(1))

  val ReasonablyComplexTree =
    Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Branch(
        Branch(
          Leaf(12),
          Leaf(6)
        ),
        Leaf(21)
      )
    )

  behavior of "size"

  it should "return 1 for a leaf" in {
    Tree.size(LeafOnly) should be (1)
  }

  it should "return 3 for a branch" in {
    Tree.size(SimpleBranch) should be (3)
  }

  it should "return proper size for more-complicated tree" in {
    Tree.size(ReasonablyComplexTree) should be (9)
  }


  behavior of "maximum"

  it should "return leaf value for leaf" in {
    Tree.maximum(LeafOnly) should be (0)
  }


  it should "return 1 for a simple branch" in {
    Tree.maximum(SimpleBranch) should be (1)
  }

  it should "return 21 for more-complicated tree" in {
    Tree.maximum(ReasonablyComplexTree) should be (21)
  }


  behavior of "depth"

  it should "return 1 for leaf" in {
    Tree.depth(LeafOnly) should be (1)
  }

  it should "return 2 for a simple branch" in {
    Tree.depth(SimpleBranch) should be (2)
  }

  it should "return 4 for more-complicated tree" in {
    Tree.depth(ReasonablyComplexTree) should be (4)
  }



  behavior of "map"

  def mapF(i: Int) = (2*i).toString

  val LeafOnlyMapped = Leaf("0")

  val SimpleBranchMapped = Branch(Leaf("0"), Leaf("2"))

  val ReasonablyComplexTreeMapped =
    Branch(
      Branch(
        Leaf("2"),
        Leaf("4")
      ),
      Branch(
        Branch(
          Leaf("24"),
          Leaf("12")
        ),
        Leaf("42")
      )
    )


  it should "map a leaf" in {
    Tree.map(LeafOnly)(mapF) should be (LeafOnlyMapped)
  }

  it should "map a simple branch" in {
    Tree.map(SimpleBranch)(mapF) should be (SimpleBranchMapped)
  }

  it should "map a more-complicated tree" in {
    Tree.map(ReasonablyComplexTree)(mapF) should be (ReasonablyComplexTreeMapped)
  }


  // NOTE! in real life this would be a mistake, copy and pasting entire tests;
  //  better to iterate over a list of (two) implementations to run the same "should" blocks;
  //  then again, in real life you probably wouldn't implement everything twice ;-)

  behavior of "sizeWithFold"

  it should "return 1 for a leaf" in {
    Tree.sizeWithFold(LeafOnly) should be (1)
  }

  it should "return 3 for a branch" in {
    Tree.sizeWithFold(SimpleBranch) should be (3)
  }

  it should "return proper size for more-complicated tree" in {
    Tree.sizeWithFold(ReasonablyComplexTree) should be (9)
  }



  behavior of "maximumWithFold"

  it should "return leaf value for leaf" in {
    Tree.maximumWithFold(LeafOnly) should be (0)
  }

  it should "return 1 for a simple branch" in {
    Tree.maximumWithFold(SimpleBranch) should be (1)
  }

  it should "return 21 for more-complicated tree" in {
    Tree.maximumWithFold(ReasonablyComplexTree) should be (21)
  }


  behavior of "depthWithFold"

  it should "return 1 for leaf" in {
    Tree.depthWithFold(LeafOnly) should be (1)
  }

  it should "return 2 for a simple branch" in {
    Tree.depthWithFold(SimpleBranch) should be (2)
  }

  it should "return 4 for more-complicated tree" in {
    Tree.depthWithFold(ReasonablyComplexTree) should be (4)
  }


  behavior of "mapWithFold"

  it should "map a leaf" in {
    Tree.mapWithFold(LeafOnly)(mapF) should be (LeafOnlyMapped)
  }

  it should "map a simple branch" in {
    Tree.mapWithFold(SimpleBranch)(mapF) should be (SimpleBranchMapped)
  }

  it should "map a more-complicated tree" in {
    Tree.mapWithFold(ReasonablyComplexTree)(mapF) should be (ReasonablyComplexTreeMapped)
  }
}
