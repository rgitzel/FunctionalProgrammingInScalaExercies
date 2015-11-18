package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions
import scala.Some

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  def lazyUnit[A](a: => A) = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map22[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => Map2ResultFuture(a(es), b(es), f)

  private case class Map2ResultFuture[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C) extends Future[C] {
    def get = {
      UnitFuture(f(af.get, bf.get)).get
    }
    def get(timeout: Long, units: TimeUnit) = {
      val timeoutInMillis = TimeUnit.MILLISECONDS.convert(timeout, units)
      val start = System.currentTimeMillis
      val av = af.get(timeoutInMillis, TimeUnit.MILLISECONDS)
      val bv = bf.get(timeoutInMillis - (System.currentTimeMillis - start), TimeUnit.MILLISECONDS)
      // should do some elapsed time check, here, and throw an exception if gone too far?
      UnitFuture(f(af.get, bf.get)).get
    }
    // not sure how to deal with these, yet
    def isDone = false
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(List[A]())){ case(acc, p) =>
      map2(acc, p)((list, v) => list :+ v)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fs = as.map(
      asyncF(a =>
        if(f(a)) Some(a) else None
      )
    )
    map2(sequence(fs), unit()){ (fs,_) =>
      fs.flatten
    }
  }

  def binaryMap[A,B](as: IndexedSeq[A], z: B, f: A => B, combine: (B,B) => B): Par[B] = {
    as.size match {
      case 0 =>
        unit(z)
      case 1 =>
        unit(f(as(0)))
      case n =>
        val (l,r) = as.splitAt(as.length/2)
        // can only use the 'fork' version if a non-fixed-thread executor service
        //  or not? maybe it's making the number of threads high enough?
        println(s"${now} split ${as} into '${l}' and '${r}'")
        map2(
          fork(binaryMap(l, z, f, combine)),
          fork(binaryMap(r, z, f, combine))
        )(combine)
        //map2(binaryMap(l, z, f, combine), binaryMap(r, z, f, combine))(combine)
    }
  }

  def parSum2(ints: IndexedSeq[Int]): Par[Int] =
    binaryMap[Int,Int](
      ints,
      0,
      i => i,
      (b1, b2) => b1 + b2
    )

  def parMax(ints: IndexedSeq[Int]): Par[Int] =
    binaryMap[Int,Int](
      ints,
      Int.MinValue,
      i => i,
      (b1, b2) => b1.max(b2)
    )

  def parConcat(ints: IndexedSeq[Int]): Par[String] =
    binaryMap[Int,String](
      ints,
      "",
      i => i.toString,
      (b1, b2) => b1 + b2
    )

  def now = System.currentTimeMillis() % 1000

  def wordCount(s: String) =
    s.trim match {
      case "" => 0
      case t => t.split("\\s+", 0).length
    }

  def parWordCountWithIndexedSeq(delay: Int, paragraphs: IndexedSeq[String]): Par[Int] =
    binaryMap[String,Int](
      paragraphs,
      0,
      paragraph => {
        println(s"${now} counting '${paragraph}'")
        Thread.sleep(delay)
        println(s"${now} done counting '${paragraph}'")
        wordCount(paragraph)
      },
      (b1, b2) => {
        println(s"${now} combining '${b1}' and '${b2}")
        b1 + b2
      }
    )

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}