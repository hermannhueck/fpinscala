package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(p: A => Boolean): Option[A] =
    flatMap(a => if (p(a)) Some(a) else None)

  def ap[B](of: Option[A => B]): Option[B] = (of, this) match {
    case (Some(f), Some(a)) => Some(f(a))
    case (_, _) => None
  }

  def isEmpty: Boolean = this == None
  def nonEmpty: Boolean = !isEmpty
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def empty[A]: Option[A] = None

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
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,Z](oa: Option[A], ob: Option[B])(f: (A, B) => Z): Option[Z] = for {
    a <- oa
    b <- ob
  } yield f(a, b)

  def map2_2[A,B,Z](oa: Option[A], ob: Option[B])(f: (A, B) => Z): Option[Z] =
    oa.flatMap(a => ob.flatMap(b => Some(f(a, b)))) // better with map

  def map2_3[A,B,Z](oa: Option[A], ob: Option[B])(f: (A, B) => Z): Option[Z] =
    oa.flatMap(a => ob.map(b => f(a, b)))

  def map2_4[A,B,Z](oa: Option[A], ob: Option[B])(f: (A, B) => Z): Option[Z] = {
    val of: Option[B => Z] = oa.map(f.curried)
    ob ap of
  }

  def map3[A,B,C,Z](oa: Option[A], ob: Option[B], oc: Option[C])(f: (A, B, C) => Z): Option[Z] = for {
    a <- oa
    b <- ob
    c <- oc
  } yield f(a, b, c)

  def map3_4[A,B,C,Z](oa: Option[A], ob: Option[B], oc: Option[C])(f: (A, B, C) => Z): Option[Z] =
    oc ap (ob ap oa.map(f.curried))

  def ap[A, B](of: Option[A => B])(oa: Option[A]): Option[B] = oa ap of

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _ map f

  def sequence[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil => Some(Nil)
    case None :: xs => None
    case Some(a) :: opts => sequence(opts) map (a :: _)
  }

  def sequence_2[A](loa: List[Option[A]]): Option[List[A]] =
    loa.foldRight(Some(Nil): Option[List[A]])(map2(_, _)(_ :: _))

  def traverse[A, B](loa: List[A])(f: A => Option[B]): Option[List[B]] =
    loa.foldRight(Some(Nil): Option[List[B]]) {
      (a, opts) => map2(f(a), opts)(_ :: _)
    }
}