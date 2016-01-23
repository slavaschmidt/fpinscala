package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => ???
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => ???
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
    }

  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((y, _) => y + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, h) => Cons(h, acc))

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a, b))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((i, acc) => Cons(i, acc))

  def flatten[A](a: List[List[A]]): List[A] = foldLeft(a, Nil:List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] =  l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def map2[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((i, acc) => Cons(f(i), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(h, t) => filter(t)(f)
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as,Nil:List[A])((i, acc) => if (f(i)) Cons(i, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter3[A](as: List[A])(f: A => Boolean) = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipInt(a1: List[Int], a2: List[Int]): List[Int] =
    if (length(a1) != length(a2)) ??? else
    a1 match {
      case Nil => Nil
      case Cons(h, t) => a2 match {
        case Cons(h2, t2) => Cons(h2 +h, zipInt(t, t2))
      }
    }


  def zipWith[A](a1: List[A], a2: List[A])(f:(A,A) => A): List[A] = (a1, a2) match {
    case (Nil, a) => a
    case (b, Nil) => b
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }
    startsWith(sup, sub) || (sup match {
      case Nil => false
      case Cons(h, t) => hasSubsequence(t, sub)
    })
  }

}

import List._

object TestList extends App {

  assert(append(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))

  assert(append2(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6), append2(List(1,2,3), List(4,5,6)))

  assert(flatten(List(List(1,2),List(3,4), List(5,6))) == List(1,2,3,4,5,6))

  assert(reverse(List(1,2,3,4,5,6)) == List(6,5,4,3,2,1))

  assert(sum3(List(1,2,3,4,5,6)) == 21)

  assert(length3(List(1,2,3,4,5,6)) == 6)

  assert(product3(List(1,2,3,4,5,6)) == 720.0, product3(List(1,2,3,4,5,6)))

  assert(map(List(1,2,3,4,5,6))(a => a + 1) == List(2,3,4,5,6,7))

  ass(map2(List(1,2,3,4,5,6))(a => a + 1), List(2,3,4,5,6,7))

  ass(filter(List(1,2,3,4,5,6))(_ % 2 == 0), List(2,4,6))

  ass(filter2(List(1,2,3,4,5,6))(_ % 2 == 0), List(2,4,6))

  ass(filter3(List(1,2,3,4,5,6))(_ % 2 == 0), List(2,4,6))

  ass(flatMap(List(1,2,3))(i => List(i,i)), List(1,1,2,2,3,3))

  ass(zipInt(List(1,2,3), List(1,2,3)), List(2,4,6))

  ass(zipWith(List(1,2,3), List(1,2,3))(_ * _), List(1,4,9))

  ass(hasSubsequence(List(1,2,3,4), List(1,2)), true)

  ass(hasSubsequence(List(1,2,3,4), List(2,3)), true)

  ass(hasSubsequence(List(1,2,3,4), List(4)), true)

  ass(hasSubsequence(List(1,2,3,4), List(1,3)), false)

  ass(foldLeft(List("a","b","c","d"), "")((a,b)=> a + b), "abcd")

  ass(Seq("a","b","c","d").foldLeft("")((a,b)=> a + b), "abcd")

  ass(Seq("a","b","c","d").foldRight("")((a,b)=> a + b), "abcd")

  ass(foldRight(List("a","b","c","d"), "")((a,b)=> a + b), "abcd")

  ass(foldRight2(List("a","b","c","d"), "")((a,b)=> a + b), "abcd")

  ass(foldLeft2(List("a","b","c","d"), "")((a,b)=> a + b), "abcd")

  def ass(a: Any, b: Any) = assert(a == b, a)
}