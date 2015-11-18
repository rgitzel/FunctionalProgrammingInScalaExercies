package fpinscala.parallelism


import java.util.concurrent.Executors

import Par._

object ParMain extends App {

  val fa: Par[Int] = lazyUnit {
    println("starting a")
    Thread.sleep(100)
    println("finished a")
    17
  }

  val fb: Par[Int] = lazyUnit {
    println("starting b")
    Thread.sleep(100)
    println("finished b")
    18
  }

  def f(a: Int, b: Int) = {
    a + b
  }

  val m = map2(fa, fb)(f)

  val es = Executors.newFixedThreadPool(3)

  println(m(es).get)
}
