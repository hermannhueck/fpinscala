package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._
import monoids._

import language.higherKinds
import language.implicitConversions
import language.reflectiveCalls

trait Applicative[F[_]] extends Functor[F] { self =>

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_)) // apply in terms of map2: =

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa) // in terms of apply + unit
    // map2(fa, unit(()))((a, _) => f(a)) // in terms of map2 + unit

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateA[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  // also as existential: Applicative[({type f[x] = (F[x], G[x])})#f] == Applicative[(F[x], G[x]) forSome {type x}]
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def unit[A](a: => A): (F[A], G[A]) =
      (self.unit(a), G.unit(a))
    override def apply[A, B](fab_gab: (F[A => B], G[A => B]))(fa_ga: (F[A], G[A])): (F[B], G[B]) =
      (self.apply(fab_gab._1)(fa_ga._1), G.apply(fab_gab._2)(fa_ga._2))
  }

  // also as existential: Applicative[({type f[x] = F[G[x]]})#f] == Applicative[(F[G[x]]) forSome {type x}]
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f] {

    override def unit[A](a: => A): F[G[A]] =
      self.unit(G.unit(a))
    override def apply[A, B](fgab: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
      self.map2(fgab, fga)(G.apply(_)(_))
    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fga, fgb)(G.map2(_, _)(f))
  }

  def sequenceMap[K, V](mkfv: Map[K,F[V]]): F[Map[K,V]] =
    mkfv.foldRight(unit(Map.empty[K, V])) { case ((k, fv), acc) =>
        map2(fv, acc)((v, m) => m + (k -> v))
    }


  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] = {
    val fbcz: F[B => C => Z] = map(fa)(f.curried)
    val fcz: F[C => Z] = apply(fbcz)(fb)
    val fz: F[Z] = apply(fcz)(fc)
    fz
  }

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] = {
    val fbcdz: F[B => C => D => Z] = map(fa)(f.curried)
    val fcdz: F[C => D => Z] = apply(fbcdz)(fb)
    val fdz: F[D => Z] = apply(fcdz)(fc)
    val fz = apply(fdz)(fd)
    fz
  }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((_, _, _))

  def tuple4[A, B, C, D](fa: F[A], fb: F[B], fc: F[C], fd: F[D]): F[(A, B, C, D)] =
    map4(fa, fb, fc, fd)((_, _, _, _))
}

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  // Kleisli composition
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fab)(f => map(fa)(a => f(a)))
}

object Monad {

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa flatMap f
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Monad composition, where the second Monad also has a Traverse instance
  def composeM[F[_], G[_]](implicit MF: Monad[F], MG: Monad[G], TG: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] =
        MF.unit(MG.unit(a))
      override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        MF.flatMap(fga)(ga => MF.map(TG.traverse(ga)(f))(MG.join))
    }
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled // Combine elements pointwise
  }

  // this instance defines the validationApplicative in terms of unit and apply
  // see the answers for an instance defined in terms of unit and map2
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = (fab, fa) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (e@Failure(_, _), Success(a)) => e
      case (Success(f), e@Failure(_, _)) => e
      case (Failure(head1, tail1), Failure(head2, tail2)) =>
        Failure(head1, tail1 ++ Vector(head2) ++ tail2)
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  def stateApplicative[S]: Applicative[({type f[x] = State[S, x]})#f] = new Applicative[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] =
      State(s => (a, s))
    override def map2[A, B, C](fa: State[S, A], fb: State[S, B])(f: (A, B) => C): State[S, C] =
      fa.map2(fb)(f)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = {
    sequence(map(fa)(f))
  }
  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(fa => fa)

  type Id[A] = A

  implicit val idApplicative: Applicative[Id] = new Applicative[Id] {
    def unit[A](a: => A): A = a
    override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = fab(fa)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => f(a): Id[B]) // uses idApplicative implicitly

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Applicative.stateApplicative)

  def zipWithIndex_[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => for {
      i <- get[Int] // Get the current state, the index.
      _ <- set(i + 1) // Add 1 and set the result as the new state.
    } yield (a, i)).run(0)._1

  def toList_[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- get[List[A]] // Get the current state, the accumulated list.
      _  <- set(a :: as) // Add the current element and set the new list as the new state.
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {

  val listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[G[_] : Applicative, A, B](la: List[A])(f: A => G[B]): G[List[B]] = {
      val G: Applicative[G] = implicitly[Applicative[G]]
      la.foldRight(G.unit(List.empty[B])) { (a, gb) =>
        G.map2(f(a), gb)(_ :: _)
      }
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](oa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G: Applicative[G] = implicitly[Applicative[G]]
      oa match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(Option.apply)
      }
    }
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[G[_] : Applicative, A, B](ta: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val G: Applicative[G] = implicitly[Applicative[G]]
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
    }
  }

  def mapTraverse[K]: Traverse[({type f[v] = Map[K, v]})#f] = new Traverse[({type f[v] = Map[K, v]})#f] {
    override def traverse[G[_] : Applicative, VA, VB](mkva: Map[K, VA])(f: VA => G[VB]): G[Map[K, VB]] = {
      val G: Applicative[G] = implicitly[Applicative[G]]
        mkva.foldRight(G.unit(Map.empty[K, VB])) { case ((k, va), gmkvb) =>
          G.map2(f(va), gmkvb)((vb, mkvb) => mkvb + (k -> vb))
        }
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])


// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
