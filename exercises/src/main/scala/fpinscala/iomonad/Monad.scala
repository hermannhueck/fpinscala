package fpinscala.iomonad

import language.higherKinds // Disable warnings for type constructor polymorphism
import language.implicitConversions

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  def map[A,B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
  def sequence_[A](sfa: Stream[F[A]]): F[Unit] = foreachM(sfa)(skip)
  def sequence_[A](fas: F[A]*): F[Unit] = sequence_(fas.toStream)
  def replicateM[A](n: Int)(fa: F[A]): F[List[A]] =
    Stream.fill(n)(fa).foldRight(unit(List[A]()))(map2(_,_)(_ :: _))
  def replicateM_[A](n: Int)(fa: F[A]): F[Unit] =
    foreachM(Stream.fill(n)(fa))(skip)
  def as[A,B](fa: F[A])(b: B): F[B] = map(fa)(_ => b)
  def skip[A](fa: F[A]): F[Unit] = as(fa)(())
  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)
  def forever[A,B](fa: F[A]): F[B] = {
    lazy val fb: F[B] = fa flatMap (_ => fb)
    fb
  }
  def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
    lazy val fu: F[Unit] = while_(a)(b)
    a flatMap (c => skip(when(c)(fu)))
  }
  def doWhile[A](fa: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a <- fa
    ok <- cond(a)
    _ <- if (ok) doWhile(fa)(cond) else unit(())
  } yield ()

  def foldM[A,B](sa: Stream[A])(z: B)(f: (B,A) => F[B]): F[B] =
    sa match {
      case h #:: t => f(z,h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }
  def foldM_[A,B](sa: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    skip { foldM(sa)(z)(f) }
  def foreachM[A](sa: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(sa)(())((u, a) => skip(f(a)))
  def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    f andThen (fb => flatMap(fb)(g))

  // syntax
  implicit def toMonadic[A](fa: F[A]): Monadic[F,A] =
    new Monadic[F,A] { val F = Monad.this; def get: F[A] = fa }
}

trait Monadic[F[_],A] {
  val F: Monad[F]
  import F._
  def get: F[A]
  private val fa: F[A] = get
  def map[B](f: A => B): F[B] = F.map(fa)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(fa)(f)
  def **[B](fb: F[B]): F[(A, B)] = F.map2(fa,fb)((_,_))
  def *>[B](fb: F[B]): F[B] = F.map2(fa,fb)((_, b) => b)
  def map2[B,C](fb: F[B])(f: (A,B) => C): F[C] = F.map2(fa,fb)(f)
  def as[B](b: B): F[B] = F.as(fa)(b)
  def skip: F[Unit] = F.skip(fa)
  def replicateM(n: Int): F[List[A]] = F.replicateM(n)(fa)
  def replicateM_(n: Int): F[Unit] = F.replicateM_(n)(fa)
}

