package fpinscala.datastructures

import javax.naming.OperationNotSupportedException

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // --> 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new OperationNotSupportedException("tail of empty List")
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, tail(l))

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    (n, l) match {
      case (n1, _) if n1 <= 0 => l
      case (_, Nil) => l
      case (n2, Cons(_, t)) => drop(t, n2 - 1)
    }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new OperationNotSupportedException("init of empty List")
      case Cons(_, Nil) => l
      case Cons(h, t) => Cons(h, init(t))
    }

  // Exercise 3.7
  // Can product, implemented using foldRight immediately halt the recursion
  // and return 0.0 if it encounters a 0.0?
  // No, it cannot! The foldRight (implemented eagerly) is first expanded and then computed

  // Exercise 3.8
  foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _)) // returns the original List --> List(1,2,3)

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // Exercise 3.11 a
  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  // Exercise 3.11 b
  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  // Exercise 3.11 c
  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, h) => acc + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  // Exercise 3.13
  // see answers

  // Exercise 3.14
  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, acc) => Cons(a, acc))

  def append3[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l1, l2)((acc, a) => Cons(a, acc))

  // Exercise 3.15
  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  // Exercise 3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil :List[Int])((a, acc) => Cons(a + 1, acc))

  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil :List[String])((a, acc) => Cons(a.toString, acc))

  // Exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil :List[B])((a, acc) => Cons(f(a), acc))

  // Exercise 3.19
  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil :List[A])((a, acc) => if (p(a)) Cons(a, acc) else acc)

  assert(filter(List(1,2,3))(_ % 2 == 0) == List(2))

  // Exercise 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil :List[B])((a, acc) => append(f(a), acc))

  assert(flatMap(List(1,2,3))(i => List(i, i)) == List(1,1,2,2,3,3))

  // Exercise 3.21
  def filter2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  assert(filter2(List(1,2,3))(_ % 2 == 0) == List(2))

  // Exercise 3.22
  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
      case (_, _) => Nil
    }

  assert(addPairwise(List(1,2,3), List(4,5,6)) == List(5,7,9))

  // Exercise 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case (_, _) => Nil
    }

  assert(zipWith(List(1,2,3), List(4,5,6))(_ + _) == List(5,7,9))

  // Exercise 3.24
  // see answers
}
