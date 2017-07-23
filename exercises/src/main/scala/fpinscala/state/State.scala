package fpinscala.state

import scala.annotation.tailrec


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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, newRng) if x >= 0 => (x, newRng)
    case (Integer.MIN_VALUE, newRng) => (0, newRng)
    case (x, newRng) => (x, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, newRng) = nonNegativeInt(rng)
    x / (Int.MaxValue.toDouble + 1) -> newRng
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (intX, newRng) = rng.nextInt
    val (doubleX, lastRng) = double(newRng)
    (intX -> doubleX, lastRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (doubleX, newRng) = double(rng)
    val (intX, lastRng) = newRng.nextInt
    (doubleX -> intX, lastRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (doubleX1, newRng1) = double(rng)
    val (doubleX2, newRng2) = double(newRng1)
    val (doubleX3, newRng3) = double(newRng2)
    ((doubleX1, doubleX2, doubleX3), newRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec def step(n: Int, list: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n > 0) {
        val (x, newRng) = rng.nextInt
        step(n - 1, x :: list, newRng)
      } else {
        (list, rng)
      }
    }

    step(count, List(), rng)
  }

  def double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.reverse.foldLeft[Rand[List[A]]](unit(List()))(map2(_, _)((l, x) => x :: l))

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def mapThroughFM[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ThroughFM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(state1 => {
      val (a, state2) = run(state1)
      f(a).run(state2)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S,A](a: A): State[S,A] = State(a -> _)

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft[State[S,List[A]]](unit(List()))((sl, sa) => sa.map2(sl)(_ :: _))

  def get[S]: State[S,S] = State(s => (s, s))

  def set[S](s: S): State[S,Unit] = State(_ => () -> s)

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  type Rand[A] = State[RNG, A]

  def updateMachine(input: Input)(machine: Machine): Machine = input match {
    case Coin if machine.candies > 0 && machine.locked =>
      Machine(locked = false, machine.candies, machine.coins + 1)
    case Turn if machine.candies > 0 && !machine.locked =>
      Machine(locked = true, machine.candies - 1, machine.coins)
    case _ => machine
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(updateMachine).map(modify))
    machine <- get
  } yield (machine.coins, machine.candies)
}