package fpinscala.state



case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(
      s => {
        val (a, next) = run(s)
        (f(a), next)
      }
    )

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(
      s => {
        val (a, next) = this.run(s)
        val (b, next2) = sb.run(next)
        (f(a, b), next2)
      }
    )

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, next) = run(s)
        f(a).run(next)
      }
    )
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}