package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(m) => m
    case Branch(l,r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => depth(l).max(depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(e) => Leaf(f(e))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B,C](t: Tree[A])(m: A => B)(r: (B, B) => B): B = t match {
    case Leaf(e) => m(e)
    case Branch(b1,b2) => r(fold(b1)(m)(r), fold(b2)(m)(r))
  }


  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)((a,b) => a + b + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)((a,b) => a.max(b))

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 1)((a,b) => a.max(b) + 1)

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)):Tree[B])((a,b) => Branch(a, b))


}

import Tree._

object TestTree extends App {

  val tree = Branch(Branch(Leaf(1), Leaf(10)), Leaf(100))

  val tree2 = Branch(Branch(Leaf(2), Leaf(20)), Leaf(200))

  ass(size(tree), 5)

  ass(size2(tree), 5)

  ass(depth(tree), 3)

  ass(depth2(tree), 3)

  ass(maximum(tree), 100)

  ass(maximum2(tree), 100)

  ass(map(tree)(a => a * 2), tree2)

  ass(map2(tree)(a => a * 2), tree2)

  def ass(a: Any, b: Any) = assert(a == b, a)

}

