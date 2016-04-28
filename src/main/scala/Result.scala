package com.crockeo.clasp

object Result {
  // Language tokens.
  sealed trait Token

  case class TQuote(t: Token) extends Token {
    override def toString(): String =
      s"'$t"
  }

  case class TAtom(s: String) extends Token {
    override def toString(): String =
      s"$s"
  }

  case class TBool(b: Boolean) extends Token {
    override def toString(): String =
      if (b)
        "#t"
      else
        "#f"
  }

  case class TChar(c: Char) extends Token {
    override def toString(): String =
      s"'$c'"
  }

  case class TString(s: String) extends Token {
    override def toString(): String =
      "\"" + s + "\""
  }

  case class TInt(n: Int) extends Token {
    override def toString(): String =
      s"$n"
  }

  case class TFloat(n: Float) extends Token {
    override def toString(): String =
      s"$n"
  }

  case class TFunction(args: List[TAtom], body: Token) extends Token {
    override def toString(): String =
      s"(lambda ${TList(args)} ${body})"
  }

  case class TList(l: List[Token]) extends Token {
    override def toString(): String = l match {
      case Nil => "()"
      case _   => l.map(_.toString).reduceLeft(_ + " " + _) + ")"
    }
  }

  // Errors to do with language evaluation. Parse errors are handled by the parser
  // themselves.
  object ClaspError {
    sealed trait Kind

    case object SyntaxError extends Kind
    case object SystemError extends Kind
    case object ValueError  extends Kind
    case object TypeError   extends Kind
  }

  class ClaspError(kind: ClaspError.Kind, ctx: String) extends Exception {
    override def toString(): String =
      s"ClaspError: ${kind.toString}: ${ctx}"
  }

  type ClaspResult = Either[ClaspError, (Token, Context)]

  // Implementing Functor & Monad over an Either.
  implicit class EitherCollection[A, B](e: Either[A, B]) {
    def map[C](f: B => C): Either[A, C] = e match {
      case Left(a)  => Left(a)
      case Right(b) => Right(f(b))
    }

    def flatMap[C](f: B => Either[A, C]): Either[A, C] = e match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }
  }
}
