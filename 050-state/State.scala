trait RNG {
  def nextInt: (Int, RNG)
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

  // Exercise 1 (CB 6.1)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (Int.MinValue, next) => (Int.MaxValue, next)
      case (value, next) => (Math.abs(value), next)
    }
  }

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = {
    rng.nextInt match {
      case (value, next) => (Math.abs(value).toDouble/Int.MaxValue, next)
    }
  }

  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i,d), r1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r1) = r.nextInt
    ((d,i), r1)
  }

  // def boolean(rng: RNG): (Boolean, RNG) =
  //  rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def f (c: Int)(r: RNG)(l: List[Int]): (List[Int], RNG) = {
      if (c <= 0) (l, r)
      else {
        val (i, r1) = r.nextInt
        f(c-1)(r1)(i::l)
      }
    }
    f(count)(rng)(List())
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)
  val _double: Rand[Double] =
    map(nonNegativeInt)(i => Math.abs(i).toDouble/Int.MaxValue)

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }

  // this is given in the book

  // def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  //  map2(ra, rb)((_, _))

  // val randIntDouble: Rand[(Int, Double)] = both(int, double)

  // val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight (unit(Nil):Rand[List[A]]) ((v, acc) => map2 (acc, v) ((acc, v) => v::acc))

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g (a) (rng2)
    }

  // Why not map?
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap (nonNegativeInt) (v => rng => (v%n, rng))
}

import State._

case class State[S, +A](run: S => (A, S)) {
  // Exercise 9 (6.10)
  def map[B](f: A => B): State[S, B] =
    State (
      s => {
        val (a, s2) = run (s)
        (f (a), s2)
      })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State (
      s => {
        val (a, s2) = run (s)
        val (b, s3) = sb.run (s2)
        (f (a, b), s3)
      })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, s2) = run (s)
        (f (a)).run(s2)
      })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))


  // Exercise 9 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight (unit(Nil):State[S, List[A]]) ((v, acc) => acc.map2 (v) ((acc, v) => v::acc))


  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def random_int :Rand[Int] =  State (_.nextInt)

   // Exercise 10
  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (a, s2) = s.run(seed)
    Stream.cons(a, state2stream (s) (s2))
  }

  // Exercise 11
  val random_integers = state2stream (random_int) (RNG.Simple(12313))
  val ten_randoms = random_integers.take(10)
}


