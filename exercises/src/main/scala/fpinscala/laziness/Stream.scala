package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) if n <= 1 => cons(h(), empty)
    case Cons(h, t) => cons(h(), t().take(n-1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case r if n == 0 => r
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case  _ => empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a,b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = foldRight(None:Option[A])((a,b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a,b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a,r) => cons(a,r))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def takeViaUnfold(m: Int): Stream[A] =
    unfold(this, m) {
      case (Empty, _) => None
      case (Cons(h, t), n) if n == 1 => Some(h(), (Empty, 0))
      case (Cons(h, t), n) => Some(h(), (t(), n-1))
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B](s2: Stream[B]): Stream[(A, B)] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Empty, Empty) => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).filter(_._2.isDefined).forAll(p => p._1 == p._2)

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    } append Stream(empty)

  // Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the intermediate results
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case s @ Cons(_, t) => Some((s.foldRight(z)(f), t()))
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  val fibs: Stream[Int] = {
    def fibs0(n2: Int, n1: Int): Stream[Int] = cons(n2, fibs0(n1, n2 + n1))
    fibs0(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }

  val ones2: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def from2(n: Int): Stream[Int] = unfold(n)(i => Some(i, i+1))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  val fibs2: Stream[Int] = unfold((0,1))(n => Some(n._1, (n._2, n._2 + n._1)))

}

object TestStream extends App {

  val s = Stream(1,2,3,4,5,6,7,8)
  val l = List(1,2,3,4,5,6,7,8)

  ass(s.toList, l)

  ass(Stream(1,2,3).takeViaUnfold(2).toList, List(1, 2))

  ass(Stream(1,2,3).drop(2).toList, List(3))

  ass(Stream(1,2,3).takeWhile(_ < 3).toList, List(1, 2))

  ass(Stream(1,2,3).forAll(_ < 4), true)

  ass(Stream(1,2,3).forAll(_ < 2), false)

  ass(Stream(1,2,3).takeWhile2(_ < 3).toList, List(1, 2))

  ass(Stream(1,2,3).headOption, Some(1))

  ass(Stream(2,3).headOption, Some(2))

  ass(Empty.headOption, None)

  ass(s.map(_ * 2).toList, l.map(_ * 2))

  ass((empty: Stream[Int]).map(_ * 2).toList, Nil)

  ass(s.filter(_ % 2 == 0).toList, List(2,4,6,8))

  ass((empty: Stream[Int]).filter(_ * 2  > 0).toList, Nil)

  ass(Stream(1,2,3,4).append(Stream(5,6,7,8)).toList, l)

  ass(constant2(3).take(5).toList, List(3,3,3,3,3))

  ass(from2(3).take(5).toList, List(3,4,5,6,7))

  ass(fibs2.take(8).toList, List(0,1,1,2,3,5,8,13))


  ass(Stream(1,2,3).scanRight(0)(_ + _).toList, List(6,5,3,0)) // List(1+2+3+0, 2+3+0, 3+0, 0)

  println(s.tails.map(_.toList.mkString(",")).toList.mkString(";"))

  def ass(a: Any, b: Any) = assert(a == b, a)
}