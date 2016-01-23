package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(r) => Right(f(r))
   case l @ Left(_) => l
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(r) => f(r)
   case l @ Left(_) => l
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case r @ Right(_) => r
   case _ => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  for { a <- this;  bb <- b } yield f(a, bb)

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldLeft(Right(Nil): Either[E, List[B]]) { (res, x) =>
      f(x) flatMap { xx => res map { rr => rr :+ xx }}
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object TestEither extends App {
  val a = List(Right("a"), Right("b"), Right("c"))
  val b = Right(List("a", "b", "c"))
  val c = List(Right("a"), Left(20), Right("c"))
  ass(Either.sequence(a), b)
  ass(Either.sequence(c), Left(20))
  def ass(a: Any, b: Any) = assert(a == b, a)
}