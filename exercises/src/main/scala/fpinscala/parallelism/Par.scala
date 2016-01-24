package fpinscala.parallelism

import java.util.concurrent._
import fpinscala.parallelism.Par.Par

import language.implicitConversions
import scala.annotation.tailrec

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
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

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map2to[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(timeout: Long, unit: TimeUnit): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      val now = System.currentTimeMillis()
      val afg = af.get(timeout, unit)
      val usedMs = System.currentTimeMillis() - now
      val left = timeout - unit.convert(usedMs, TimeUnit.MILLISECONDS)
      if (left <= 0) throw new TimeoutException
      UnitFuture(f(af.get, bf.get(left, unit)))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft(unit(Nil: List[A]))((acc,x) => map2(acc,x)((a,b) => a :+ b))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val fas = as.map(asyncF(a => if (f(a)) List(a) else Nil))
    val s = sequence(fas)
    // this step is not done in parallel
    // so the parFilter is useful only in the case if calling f is expensive
    // otherwise the overhead of flattening will negate the benefit of parallelism
    map(s)(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
        val (l,r) = ints.splitAt(ints.length/2)
        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }


  def reduce[A,B](as: List[A])(m: A => B)(re: (B,B) => B): Par[B] =
    if (as.length <= 1)
      Par.unit(m(as.head))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2to(Par.fork(reduce(l)(m)(re)), Par.fork(reduce(r)(m)(re)))(re)(10, TimeUnit.MILLISECONDS)
    }


  def parMax(as: IndexedSeq[Int]): Par[Int] =
    reduce(as.toList)(a => a) { (a, b) => if (a > b) a else b }

  def parWordCount(as:List[String]): Par[Int] =
    reduce(as)(_.split("\\s+").length)(_ + _)

  // in terms of map2:

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = {
    val fc = f.curried
    val im = map2(a,b)((xa,xb) => fc(xa)(xb))
    map2(c, im)((xc, xim) => xim(xc))
  }

  def map4[A,B,C,D,E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A,B,C,D) => E): Par[E] = {
    val fc = f.curried
    val im = map3(a,b,c)((xa,xb,xc) => fc(xa)(xb)(xc))
    map2(d, im)((xd, xim) => xim(xd))
  }

  def map5[A,B,C,D,E,F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A,B,C,D,E) => F): Par[F] = {
    val fc = f.curried
    val im = map3(a,b,c)((xa,xb,xc) => fc(xa)(xb)(xc))
    map3(d,e,im)((xd, xe, xim) => xim(xd)(xe))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      choices.apply(run(es)(n).get)(es)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    def bool2int(cond: Par[Boolean]) = map(cond)(if (_) 0 else 1)
    choiceN(bool2int(cond))(List(t,f))
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      choices(k)(es)
    }

  def choiceGen[A, B](selector: Par[A])(selection: A => Par[B]): Par[B] = es => {
    val sel = run(es)(selector).get
    selection(sel)(es)
  }

  def choiceViaGen[A](n: Par[Boolean])(a: Par[A], b: Par[A]): Par[A] = choiceGen(n)(c => if (c) a else b)

  def choiceNViaGen[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = choiceGen(n)(choices.apply)

  def choiceMapViaGen[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = choiceGen(key)(choices.apply)

  // 100% match :)
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = choiceGen(pa)(choices)

  def join[A](a: Par[Par[A]]): Par[A] = es =>
    run(es)(run(es)(a).get())

  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = join(map(pa)(choices))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(b => b)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}

/*

  Ex. 7.7

  Given: map(y)(id) == y
  Prove: map(map(y)(g))(f) == map(y)(f compose g)

  0. TODO: Read "Theorems for free!"


  Ex. 7.8

  fork(x) == x

  Revisit your implementation of fork and try to find a counterexample or convince yourself that the law holds for your implementation.


  1. RejectedExecutionException is thrown by the submit method of the ExecutorService which could break the law
  2. There are bounded an unbounded sizes of the thread pools in different implementations.
      With fixed size there could be a problems forking a computation which could lead to the deadlock

  Ex. 7.9

  Show that any fixed-size thread pool can be made to deadlock given this implementation of fork.

  Given the size of the pool == N, the idea is to start n+1 computations.
  The computation k should depend on computation k+1 in order to finish.


  Ex. 7.10

  Our non-blocking representation doesn’t currently handle errors at all. If at any point our computation throws an exception, the run implementation’s latch never counts down and the exception is simply swallowed. Can you fix that?

  1. The minimally intrusive change would be to add a timeout to countdown latch and a default "error" value for run method.
    This is not type-safe
  2. TODO


 */

object Ex7_9 extends App {
  import Par._

  @tailrec
  def wrap[A](n: Int, start: Par[A]): Par[A] =
    if (n == 0) start
    else wrap(n-1, fork(start))

  val sizeOfPool = 25
  val a = lazyUnit(sizeOfPool)
  val S = Executors.newFixedThreadPool(sizeOfPool)

  println(Par.equal(S)(a, wrap(sizeOfPool, a)))

  S.shutdown()
}

object TestBlocking extends App with Text {
    import Par._
    val S = Executors.newCachedThreadPool()
    val in = (1 to 10).toIndexedSeq
    println(run(S)(parMax(in)).get)

    println(run(S)(parWordCount(text)).get)
    S.shutdown()
}

trait Text {
  val text = """It is important that the equals method for an instance of Ordered[A] be consistent with the compare method. However, due to limitations inherent in the type erasure semantics, there is no reasonable way to provide a default implementation of equality for instances of Ordered[A]. Therefore, if you need to be able to use equality on an instance of Ordered[A] you must provide it yourself either when inheriting or instantiating.
               |
               |It is important that the hashCode method for an instance of Ordered[A] be consistent with the compare method. However, it is not possible to provide a sensible default implementation. Therefore, if you need to be able compute the hash of an instance of Ordered[A] you must provide it yourself either when inheriting or instantiating.
               |""".split("\n").toList
}