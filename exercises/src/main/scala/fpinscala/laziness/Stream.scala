package fpinscala.laziness

trait Stream[+A] {

  import Stream._

  // Exercise 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  // Here `b` is the unevaluated recursive step that folds the tail of the stream.
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.1
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def exists2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  // Exercise 5.6
  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case _ => None
  }

  def headOption2: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  // Exercise 5.7: map, filter, append, flatMap using foldRight.
  // Part of the exercise is writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] = // non-strict s
    foldRight(s)((b, acc) => cons(b, acc))

  // Exercise 5.13: Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n_) if n_ > 1 => Some((h(), (t(), n_ - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  // Exercise 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  // Exercise 5.16
  // The function can't be implemented using `unfold`,
  // since `unfold` generates elements of the `Stream` from left to right.
  // It can be implemented using `foldRight` though.
  // The implementation is just a `foldRight` that keeps the accumulated value
  // and the stream of intermediate results, which we `cons` onto during each iteration.
  // When writing folds, it's common to have more state in the fold than is needed to compute the result.
  // Here, we simply extract the accumulated list once finished.
  //
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  // Exercise 5.8
  def constant[A](const: A): Stream[A] =
    Stream.cons(const, constant(const)) // strict

  def constant2[A](a: A): Stream[A] = { // non-strict
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  val fibs: Stream[Int] = {

    def go(prev: Int, curr: Int): Stream[Int] =
      cons(prev, go(curr, prev + curr))

    go(0, 1)
  }

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((h, s)) => cons(h, unfold(s)(f))
    }

  // Exercise 5.12
  def onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  def constViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (prev, curr) => Some((prev, (curr, prev + curr))) }
}