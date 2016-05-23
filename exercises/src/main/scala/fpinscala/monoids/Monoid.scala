package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds
import scala.annotation.tailrec

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    override def zero: A => A = identity
  }

  import fpinscala.testing._
  import Prop._
  private def triple[A](gen: Gen[A]): Gen[(A,A,A)] = for {
    a1 <- gen
    a2 <- gen
    a3 <- gen
  } yield (a1, a2, a3)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(triple(gen)) { case (a, a1, a2) =>
      m.op(m.op(a1, a2), a) == m.op(a1, m.op(a2, a)) && (m.op(m.zero, a) == m.op(a, m.zero) == a)
    }

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero){ case (b, a) => m.op(b, f(a)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def dual[B](m: Monoid[B]) = new Monoid[B] {
    override def op(a1: B, a2: B): B = m.op(a2, a1)
    override def zero = m.zero
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val ff: A => B => B = a => b => f(b,a)
    foldMap(as, dual(endoMonoid[B]))(ff)(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.zero
      case 1 => f(as.head)
      case l =>
        val (p1, p2) = as.splitAt(l/2)
        m.op(foldMapV(p1, m)(f), foldMapV(p2, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type TwoOrdered = Option[(Int, Int, Boolean)]
    val m = new Monoid[TwoOrdered] {
      override def op(a1: TwoOrdered, a2: TwoOrdered): TwoOrdered = (a1, a2) match {
        case (_, None) => None
        case (None, _) => None
        case (Some((min1, max1, ordered1)), Some((min2, max2, ordered2))) =>
          Some(min1 min min2, max1 max max2, ordered1 && ordered2 && max1 <= min2)
      }
      override def zero: TwoOrdered = None
    }
    foldMapV(ints, m)(a => Some((a, a, true))).forall(_._3)
  }


  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC


  import fpinscala.parallelism.Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)
      override def zero: Par[A] = Par.unit(m.zero)
    }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(Par.asyncF(f))

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(s1,c1,e1), Part(s2,c2,e2)) => Part(s1,c1+c2+1,e2)
      case (Stub(a), Stub(b)) => Stub(a+b)
      case (Part(s1,c1,e1), Stub(b)) => Part(s1,c1,e1+b)
      case (Stub(a), Part(s2,c2,e2)) => Part(a+s2,c2,e2)

    }
    override def zero: WC = Stub("")
  }

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "") else Stub(s)
    def cnt(s: String): Int =
      if (s.nonEmpty) 1 else 0
    val result = foldMapV(s.toList.toIndexedSeq, wcMonoid)(wc)

    result match {
      case s: Stub => cnt(s.chars)
      case Part(b,c,e) => cnt(b) + c + cnt(e)
    }
  }

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A,B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
    override def zero: (A, B) = (a.zero, b.zero)
  }

  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => b.op(a1(a), a2(a))
    override def zero: (A) => B = a => b.zero
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    IndexedSeqFoldable.foldMap(as)(a => Map(a -> 1))(mapMergeMonoid(intAddition))

}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
    val ff: A => B => B = a => b => f(a,b)
    foldMap(as)(ff)(endoMonoid[B])(z)
  }

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    val ff: A => (B => B) = a => b => f(b,a)
    foldMap(as)(ff)(dual(endoMonoid[B]))(z)
  }

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = {
    def m = new Monoid[List[A]] {
      override def op(a1: List[A], a2: List[A]): List[A] = a1 ::: a2
      override def zero: List[A] = Nil
    }
    foldMap(as)(a => List(a))(m)
  }

}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b,a) => mb.op(b,f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(value) => f(value)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(value) => f(z, value)
    case Branch(left, right) =>
      val r = foldLeft(right)(z)(f)
      foldLeft(left)(r)(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(value) => f(value, z)
    case Branch(left, right) =>
      val l: B = foldRight(left)(z)(f)
      foldRight(right)(l)(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(value) => f(value)
  }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =as match {
    case None => z
    case Some(value) => f(z, value)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =as match {
    case None => z
    case Some(value) => f(value, z)
  }
}

object Applic extends App {
  println(Monoid.bag(Vector("a", "rose", "is", "a", "rose")))
}