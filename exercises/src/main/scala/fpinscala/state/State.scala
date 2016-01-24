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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def map_[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(r => unit(f(r)))

  def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map_(rb)(f(a,_)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rc =>  {
      val (a, r1) = ra(rc)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A])) { (f, acc) =>
      map2(f, acc) { _ :: _ }
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, state) = rng.nextInt
    val res = math.abs(i+1)
    (res, state)
  }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ /(Int.MaxValue+1))

  def double(rng: RNG): (Double, RNG) = {
    val (i, s) = nonNegativeInt(rng)
    ((i / Int.MaxValue).toDouble, s)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,_) = rng.nextInt
    val (d,s) = double(rng)
    ((i,d),s)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (i,_) = rng.nextInt
    val (d,s) = double(rng)
    ((d,i),s)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,_) = double(rng)
    val (d2,_) = double(rng)
    val (d3,s) = double(rng)
    ((d1,d2,d3),s)
  }

  def ints(count: Int)(rng: RNG): Rand[List[Int]] = {
    val rnds = List.fill(count)(int)
    sequence(rnds)
  }



}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(sa => State.unit(f(sa)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(sa => sb.map(f(sa, _)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int)
object State extends App {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S,A](saas: List[State[S,A]]): State[S,List[A]] =
    saas.foldRight(unit(List.empty[A]): State[S,List[A]]) { (sa, acc) =>
      sa.map2(acc) { _ :: _ }
    }

  def operate = (i: Input) => (s: Machine) => i match {
    case _ if s.candies == 0 => s
    case Coin if s.locked => s.copy(locked = false, coins = s.coins + 1)
    case Turn if !s.locked => s.copy(locked = true, candies = s.candies - 1)
    case _ => s
  }

  def simulateMachine(inputs: List[Input]): Unit = {
    val machine = Machine(locked = true, 5, 10)
    val result = inputs.foldLeft(machine) { (a, b) => operate(b)(a) }
    println(result)
  }
}
