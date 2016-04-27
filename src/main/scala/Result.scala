package com.crockeo.clasp

object Result {
  // Language tokens.
  sealed trait Token

  case class TQuote(t: Token)                          extends Token
  case class TAtom(s: String)                          extends Token
  case class TChar(c: Char)                            extends Token
  case class TString(s: String)                        extends Token
  case class TInt(n: Int)                              extends Token
  case class TFloat(n: Float)                          extends Token
  case class TFunction(args: List[TAtom], body: Token) extends Token
  case class TList(l: List[Token])                     extends Token

  // Errors to do with language evaluation. Parse errors are handled by the parser
  // themselves.
  object ClaspError {
    sealed trait Kind

    case object SyntaxError extends Kind
    case object SystemError extends Kind
    case object ValueError  extends Kind
    case object TypeError   extends Kind
  }

  class ClaspError(kind: ClaspError.Kind, ctx: String) extends Exception { }

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
