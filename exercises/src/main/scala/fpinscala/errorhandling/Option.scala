package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(_ => this).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = if (map(f) == Some(true)) this else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) map { m => xs.map { x => math.pow(x - m, 2) } } flatMap mean

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap { ao => b map { bo => f(ao, bo) } }

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldLeft(Some(Nil): Option[List[A]]) { (r, i) =>
      r flatMap { ro => i map { ro :+ _ } }
    }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldLeft(Some(Nil): Option[List[B]]) { (r, i) =>
      r flatMap { ro => f(i) map { ro :+ _ }}
    }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a)

}

import Option._

object TestList extends App {
  val a = List(Some("a"), Some("b"), Some("c"))
  val b = Some(List("a", "b", "c"))
  val c = List(Some("a"), None, Some("c"))
  ass(sequence(a), b)
  ass(sequence(c), None)
  def ass(a: Any, b: Any) = assert(a == b, a)
}