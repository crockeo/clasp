package com.crockeo.clasp

import scala.util.parsing.combinator._
import scala.util.parsing.input.Reader

// The language definition as well as its parsing.
object Language extends RegexParsers {
  import Result._

  // Parsing out a quote.
  lazy val quote: Parser[TQuote] =
    "'" ~ clasp ^^ { case ~(_, t) => TQuote(t) }

  // Parsing out a singular atom.
  lazy val atom: Parser[TAtom] =
    "([a-zA-Z]+([a-zA-Z]|\\d|-|_)*|\\+|-|\\*|/|\\[\\]|=|\\||&|\\^|!|>|<)".r ^^ { TAtom(_) }

  // Parsing out a boolean.
  lazy val bool: Parser[TBool] =
    "#t" ^^ { _ => TBool(true) } |
    "#f" ^^ { _ => TBool(false) }

  // Parsing out a 
  lazy val char: Parser[TChar] =
    "'.'".r ^^ { s => TChar(s.charAt(1)) }

  // Parsing a quoted string.
  lazy val string: Parser[TString] =
    "\".+\"".r ^^ { s => TString(s.substring(1, s.length - 1)) }

  // Parsing out a raw integer (without casting it to anything).
  lazy val intRaw: Parser[Int] =
    "(-?[1-9][0-9]*)|0".r ^^ { _.toInt }

  // Parsing out a whole integer.
  lazy val int: Parser[TInt] =
    intRaw ^^ { TInt(_) }

  // Parsing out a floating point number (which can also be a whole number).
  lazy val float: Parser[TFloat] =
    intRaw ~ "." ~ "[0-9]+".r ^^ { case ~(~(a, _), b) => TFloat(a.toFloat + (0.1f * b.toFloat)) }

  // A series of spaced values that *only* capture atoms.
  lazy val atomSpaced: Parser[List[TAtom]] = (for {
    v1 <- atom
    v2 <- atomSpaced
  } yield v1 :: v2) | "" ^^ { _ => Nil }

  // A list of space-separated atoms.
  lazy val atomList: Parser[List[TAtom]] =
    "(" ~> atomSpaced <~ ")"

  // A series of spaced values that capture all clasp tokens.
  lazy val spaced: Parser[List[Token]] = (for {
    v1 <- clasp
    v2 <- spaced
  } yield v1 :: v2) | "" ^^ { _ => Nil }

  // Parsing out a lambda function.
  lazy val function: Parser[TFunction] =
    ("(" ~> "lambda" ~> atomList ~ clasp <~ ")") ^^ { case ~(l, t) => TFunction(l, t) }

  // A list.
  lazy val list: Parser[TList] =
    "(" ~ spaced ~ ")" ^^ { case ~(~(_, a), _) => TList(a) }

  lazy val clasp: Parser[Token] =
    quote |
    atom |
    bool |
    string |
    float |
    int |
    function |
    list

  // Reducing a ParseResult to its value, throwing an exception on failure.
  private def reduceResult[T](pr: ParseResult[T]): T = pr match {
    case Success(t, _) => t
    case Failure(e, _) => throw new Exception(s"Failed to parse: $e.")
    case Error  (e, _) => throw new Exception(s"Failed to parse: $e.")
  }

  // The "none" value in the language.
  val none: Token =
    TList(List())

  // Short-hand for parsing Tokens.
  def parse(in: CharSequence): Token =
    reduceResult(parse(clasp, in))
  def parse(in: Reader[Char]): Token =
    reduceResult(parse(clasp, in))

  // Short-hand for parsing a set of tokens in.
  def parseSet(in: CharSequence): List[Token] = parse(clasp, in) match {
    case Success(t, r) => t :: parseSet(r)
    case _             => Nil
  }

  def parseSet(in: Reader[Char]): List[Token] = parse(clasp, in) match {
    case Success(t, r) => t :: parseSet(r)
    case _             => Nil
  }

  // Reducing the result from a parseSafe call into something usable.
  private def reduceSafeParse(pr: ParseResult[Token]): Either[ClaspError, List[Token]] = pr match {
    case Success(t, r) if (r.atEnd) => Right(List(t))
    case Success(t, r)              =>
      parseSafe(r) match {
        case Left(err) => Left(err)
        case Right(ts) => Right(t :: ts)
      }

    case Failure(t, _) =>
      Left(new ClaspError(ClaspError.SyntaxError, "Failed to parse: " + t))
    case Error  (t, _) =>
      Left(new ClaspError(ClaspError.SyntaxError, "Failed to parse: " + t))
  }

  // Safely parsing out a list of tokens (returning errors).
  def parseSafe(in: CharSequence): Either[ClaspError, List[Token]] =
    reduceSafeParse(parse(clasp, in))

  // Safely parsing out a list of tokens (returning errors).
  def parseSafe(in: Reader[Char]): Either[ClaspError, List[Token]] =
    reduceSafeParse(parse(clasp, in))
}
