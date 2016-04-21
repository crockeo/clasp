package com.crockeo.clasp

import scala.util.parsing.combinator._

// The language definition as well as its parsing.
object Language extends RegexParsers {
  // Language tokens.
  sealed trait Token

  case class TAtom(s: String)                          extends Token
  case class TString(s: String)                        extends Token
  case class TInt(n: Int)                              extends Token
  case class TFloat(n: Float)                          extends Token
  case class TFunction(args: List[TAtom], body: Token) extends Token
  case class TList(l: List[Token])                     extends Token

  // Parsing out a singular ato.
  lazy val atom: Parser[TAtom] =
    "'[a-zA-Z]+".r ^^ { s => TAtom(s.substring(1)) } |
    "[a-zA-Z]+".r ^^ { TAtom(_) }

  // Parsing a quoted string.
  lazy val string: Parser[TString] =
    "\"\\w+\"".r ^^ { s => TString(s.substring(1, s.length - 1)) }

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
    atom |
    string |
    float |
    int |
    function |
    list
}
