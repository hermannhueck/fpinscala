package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {

  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(pa: Par[A]): Future[A] = pa(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      // This implementation of `map2` does _not_ respect timeouts. It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`.
      // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`,
      // then subtracts that time from the available time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))
    }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one,
  // the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool,
  // or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](pa: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = pa(es).get
    })

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  // Exercise 7.5
  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (pas.isEmpty)
      unit(Vector())
    else if (pas.length == 1)
      map(pas.head)(a => Vector(a))
    else {
      val (l, r) = pas.splitAt(pas.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(pas.toIndexedSeq))(_.toList)

  // ~= traverse
  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = {
    val pbs: List[Par[B]] = as.map(asyncF(f))
    sequence(pbs)
  }

  def parTraverse[A, B](as: List[A])(f: A => Par[B]): Par[List[B]] = {
    val pbs: List[Par[B]] = as.map(f)
    sequence(pbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map asyncF { a => if (f(a)) List(a) else List() }
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

//  def parMax[A: Ordering](l: List[Int]): Par[Option[Int]] = l match {
//    case Nil => unit(None)
//    case x :: Nil => unit(Some(x))
//    case x :: xs => parMax(xs) map asyncF (m => if (m > x) m else x)
//  }

  def parWordCount(l: List[String]): Par[Int] = ???

  def tuple2[A, B](pa: Par[A], pb: Par[B]): Par[(A, B)] = // same as Semigroup#product
    map2(pa, pb)((_, _))

  def tuple3[A, B, C](pa: Par[A], pb: Par[B], pc: Par[C]): Par[(A, B, C)] =
    map2(pa, tuple2(pb, pc))((a, pbc) => (a, pbc._1, pbc._2))

  def tuple4[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D]): Par[(A, B, C, D)] =
    map2(tuple2(pa, pb), tuple2(pc, pd))((ab, cd) => (ab._1, ab._2, cd._1, cd._2))

  def map3[A, B, C, Z](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => Z): Par[Z] =
    map(tuple3(pa, pb, pc)) { case (a, b, c) => f(a, b, c) }

  def map4[A, B, C, D, Z](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => Z): Par[Z] =
    map(tuple4(pa, pb, pc, pd)) { case (a, b, c, d) => f(a, b, c, d) }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(pa1: Par[A], pa2: Par[A]): Par[A] = es =>
    if (run(es)(cond).get) // Notice we are blocking on the result of `cond`.
      pa1(es)
    else
      pa2(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val index: Int = run(es)(n).get
    run(es)(choices(index))
  }

  def choice2[A](cond: Par[Boolean])(onTrue: Par[A], onFalse: Par[A]): Par[A] =
    choiceNViaFlatMap(map(cond)(b => if(b) 0 else 1))(List(onTrue, onFalse))

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => { // that is really flatMap
    val a = run(es)(pa).get
    run(es)(choices(a))
  }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa).get
    run(es)(f(a))
  }

  def choiceViaFlatMap[A](cond: Par[Boolean])(onTrue: Par[A], onFalse: Par[A]): Par[A] = for {
    b <- cond
    result <- if (b) onTrue else onFalse
  } yield result

  def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = for {
    index <- n
    result <- choices(index)
  } yield result

  def join[A](ppa: Par[Par[A]]): Par[A] = es => {
    val pa: Par[A] = run(es)(ppa).get()
    run(es)(pa)
  }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(pa.map(f))

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    ppa flatMap identity

  def map2ViaFlatMap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = for {
    a <- pa
    b <- pb
  } yield f(a, b)



  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
    def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
