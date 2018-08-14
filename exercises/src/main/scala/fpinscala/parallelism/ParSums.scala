package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future}

object ParSums {

  object sequential {

    def sum(ints: Seq[Int]): Int =
      ints.foldLeft(0)(_ + _)

    val result: Int = sum((0 until 10).toVector)
  }

  object split {

    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.length == 1)
        ints.headOption getOrElse 0
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        sum(l) + sum(r)
      }

    val result: Int = sum((0 until 10).toVector)
  }

  object unitGet {

    type Par[A] = ExecutorService => Future[A]
    object Par {
      def unit[A](a: => A): Par[A] = ???
      def get[A](pa: Par[A]): A = ???
    }

    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.length == 1)
        ints.headOption getOrElse 0
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        val sumL: Par[Int] = Par.unit(sum(l))
        val sumR: Par[Int] = Par.unit(sum(r))
        Par.get(sumL) + Par.get(sumR)
      }

    val result: Int = sum((0 until 10).toVector)
  }

  object notReferentiallyTransparent {

    type Par[A] = ExecutorService => Future[A]
    object Par {
      def unit[A](a: => A): Par[A] = ???
      def get[A](pa: Par[A]): A = ???
    }

    def sum(ints: IndexedSeq[Int]): Int =
      if (ints.length == 1)
        ints.headOption getOrElse 0
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.get(Par.unit(sum(l))) + Par.get(Par.unit(sum(r)))
      }

    val result: Int = sum((0 until 10).toVector)
  }

  object map2 {

    type Par[A] = ExecutorService => Future[A]
    object Par {
      def unit[A](a: => A): Par[A] = ???
      def get[A](pa: Par[A]): A = ???
      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
    }

    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length == 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(sum(l), sum(r))(_ + _)
      }

    val par: Par[Int] = sum((0 until 10).toVector)
    val result: Int = Par.get(par)
  }

  object fork {

    type Par[A] = ExecutorService => Future[A]
    object Par {
      def unit[A](a: A): Par[A] = ???
      def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???
      def fork[A](pa: => Par[A]): Par[A] = ???
      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
      def run[A](pa: Par[A]): A = ???
    }

    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.length == 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }

    val par: Par[Int] = sum((0 until 10).toVector)
    val result: Int = Par.run(par)
  }
}
