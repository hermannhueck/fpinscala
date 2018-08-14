package fpinscala.parsing

import language.{higherKinds, implicitConversions}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  type ParseError

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    ???

  implicit def string(s: String): Parser[String] = ???
  implicit def operators[A](p: Parser[A]): ParserOps[A] =
    ParserOps(p)
  implicit def asStringParser[A](a: A)(f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = p or p2
  }

  object Laws {

    val l1: Char => Boolean = { c: Char =>
      run(char(c))(c.toString) == Right(c)
    }

    val l2: String => Boolean = {
      s: String => run(string(s))(s) == Right(s)
    }

    val l3: Boolean = run(string("abra") | string("cadabra"))("abra") == Right("abra")
    val l4: Boolean = run(string("abra") | string("cadabra"))("cadabra") == Right("cadabra")

    val l5: String => Boolean = {
      s: String => run(s)(s) == Right(s) // due to implicit conversion: implicit def string
    }

    val l6: Boolean = run(or("abra", "cadabra"))("abra") == Right("abra")
    val l7: Boolean = run(or("abra", "cadabra"))("cadabra") == Right("cadabra")
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col: Int = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}