package fpinscala.testing

import java.util.concurrent.{Executors, ExecutorService}

import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par.Par
import fpinscala.state._
import fpinscala.testing.Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Proved extends Result {
    override def isFalsified = false
  }

  case object Passed extends Result {
    override def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop.forAll(g.forSize)(f)

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
    case Proved =>
      println(s"+ OK, proved property.")
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }


}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (m, n, rng) =>
    val thisResult = this.run(m, n, rng)
    if (thisResult.isFalsified) thisResult else p.run(m, n, rng)
  }

  def ||(p: Prop): Prop = Prop { (m, n, rng) =>
    val thisResult = this.run(m, n, rng)
    if (!thisResult.isFalsified) thisResult else p.run(m, n, rng)
  }
}


object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val smple: State[RNG, Int] = State(RNG.nonNegativeInt).map(_ % (stopExclusive - start) + start)
    Gen(smple)
  }

  def chooseDbl: Gen[Double] = Gen(State(RNG.double))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap {
    if (_) g1 else g2
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val r = g1._2.abs / (g1._2.abs + g2._2.abs)
    chooseDbl.flatMap(w => if (w < r) g1._1 else g2._1)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  //TODO  EXERCISE 8.19 Gen[A => B]
}

//
//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {
//
//}

object SGen {
  def choose(start: Int, stopExclusive: Int): SGen[Int] = Gen.choose(start, stopExclusive).unsized

  def chooseDbl = Gen.chooseDbl.unsized

  def unit[A](a: => A) = Gen.unit(a).unsized

  def boolean = Gen.boolean.unsized

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(1 max n, g))

}

case class SGen[A](forSize: Int => Gen[A])

case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap { n: Int =>
      this.flatMap { a: A =>
        Gen.unit(List.fill(n)(a))
      }
    }

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))

}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

object Test8 extends App {

  import Prop._
  import Gen._

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val listSorted = forAll(SGen.listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    (ns.size < 2 && sorted == ns) || (ns.size >= 2 && sorted.sliding(2).forall(l => l.head <= l.last))
  }

  run(maxProp)
  run(listSorted)

  val ES: ExecutorService = Executors.newCachedThreadPool

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

  val p3 = Prop.check {
    equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  val S = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> .75, unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case s ** a => f(a)(s).get }

  val pint: Gen[Par.Par[Int]] = Gen.choose(0, 10) map Par.unit
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  run(p3)

  run(p4)

  def parGen[A](zero: A, fold: (A, A) => A, parallelism: Int = 20)(g: Gen[A]): Gen[Par.Par[A]] =
    g.listOfN(choose(0, parallelism)).map { l =>
      l.foldLeft(Par.unit(zero)) { (p, i) =>
        Par.fork {
          Par.map2(p, Par.unit(i))(fold)
        }
      }
    }

  def parGenInt = parGen[Int](0, _ + _)(Gen.choose(100, 500))

  // EXERCISE 8.17
  val fork = forAllPar(parGenInt)(n => equal(Par.fork(n), n))

  run(fork)

  S.map(_.shutdown())
}