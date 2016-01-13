package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s){ a => unit(f(a)) }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val next = rng.nextInt
    // can't just use Math.abs, as it doesn't account for MinValue being 'bigger' than MaxValue
    val nextInt = if (next._1 < 0) -(next._1 + 1) else next._1
    (nextInt, next._2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)
  def nonNegativeOdd = map(nonNegativeEven)(_ + 1)

  def double(rng: RNG): (Double, RNG) = {
    val next = nonNegativeInt(rng)
    (next._1.toDouble / (Int.MaxValue.toDouble + 1), next._2)
  }

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(a => a.toDouble / (Int.MaxValue.toDouble + 1))
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val intNext = rng.nextInt
    val doubleNext = double(intNext._2)
    ((intNext._1, doubleNext._1), doubleNext._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val doubleNext = double(rng)
    val intNext = doubleNext._2.nextInt
    ((doubleNext._1, intNext._1), intNext._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val one = double(rng)
    val two = double(one._2)
    val three = double(two._2)
    ((one._1, two._1, three._1), three._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val head = rng.nextInt
      val tail = ints(count - 1)(head._2)
      (head._1 +: tail._1, tail._2)
    }

  //  def rngStream(rng: RNG): Stream[Int] = {
  //    val next = rng.nextInt
  //    Stream.cons(next._1, rngStream(next._2))
  //  }
  //
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil =>
        rng => {
          (List(), rng)
        }
      case head :: tail =>
        rng => {
          val (next, nextRng) = head(rng)
          val rest = sequence(tail)(nextRng)
          (next +: rest._1, rest._2)
        }
    }

  def sequenceViaFold[A](fs: List[Rand[A]]): Rand[List[A]] =
    fng => fs.foldLeft((List[A](), fng)){ case((list, ng), f) =>
      val (next, nextRng) = f(ng)
      (list :+ next, nextRng)
    }

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if(i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
}

