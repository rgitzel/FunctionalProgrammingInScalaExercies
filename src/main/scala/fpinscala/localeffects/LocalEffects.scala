package fpinscala.localeffects


object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)
  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }
  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A) = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S) = (memo, s)
    }
  }
  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: => A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S,A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S,List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    if(xs.size > 0)
      write(xs.head._1, xs.head._2).flatMap { _ =>
        fill(xs.tail)
      }
    else
      ST(())

// this works for a map of two items... so how iterate over the map in the context of the array?
//      for {
//        _ <- write (xs.head._1, xs.head._2)
//        _ <- write (xs.tail.head._1, xs.tail.head._2)
//      } yield ()
  }

  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  // Construct an array of the given size filled with the value v
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = Array.fill(sz)(v)
    })

  def fromList[S,A:Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })
}

object Immutable {
  def noop[S] = ST[S,Unit](())

  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = ???

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S, Unit] = ???

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
  })
}

import scala.collection.mutable.HashMap


sealed abstract class STMap[S,A,B] {
  protected def internalMap: HashMap[A,B]

  def size: ST[S,Int] = ST(internalMap.size)

  def write(a: A, b: B): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S) = {
      internalMap(a) = b
      ((), s)
    }
  }

  def read(a: A): ST[S,B] = ST(internalMap(a))

  def freeze: ST[S,Map[A,B]] = ST(internalMap.toMap)
}

object STMap {
  def fromMap[S,A,B](m: Map[A,B]): ST[S, STMap[S,A,B]] =
    ST(new STMap[S,A,B] {
      lazy val internalMap = {
        val hm = new HashMap[A,B]()
        m.foreach{ case(a,b) =>
          hm(a) = b
        }
        hm
      }
    })
}
