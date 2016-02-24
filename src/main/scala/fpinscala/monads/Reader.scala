package fpinscala.monads


/*
 * so what does this mean?  it's a generic converter?  turns strings into ints?  ints into strings?
 */
case class Reader[R, A](run: R => A)


object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] =
      Reader(_ => a)

    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = {
      Reader(
        r => {
          val a = st.run(r)
          f(a).run(r)  // it compiles, but... does it make sense?  'r' is 'read' twice?
        }
      )
    }
  }
}

