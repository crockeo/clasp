package com.crockeo.clasp

import org.scalatest._

// Testing language parsing.
class LanguageParseTests extends FunSuite {
  import Language._

  // Checking that a parser is correct.
  def correctParser[T](p: Parser[T])(s: String, t: T): Boolean =
    parse(p, s) match {
      case Success(v, _) => v == t
      case _             => false
    }

  test("quote") {
    val p = correctParser(quote)_
    assert(p("'test", TQuote(TAtom("test"))))
  }

  test("atom") {
    val p = correctParser(atom)_

    assert(p("test", TAtom("test")))
    assert(!p("\"test\"", TAtom("test")))
  }

  test("string") {
    val p = correctParser(string)_

    assert(p("\"test\"", TString("test")))
    assert(!p("\"ayy lmao", TString("ayy lmao")))
    assert(!p("ayy lmao", TString("ayy lmao")))
  }

  test("int") {
    val p = correctParser(int)_

    assert(p("5", TInt(5)))
    assert(p("-10", TInt(-10)))
    assert(!p("-0", TInt(0)))
  }

  test("float") {
    val p = correctParser(float)_

    assert(p("5.0", TFloat(5)))
    assert(p("5.5", TFloat(5 + (0.1f * 5))))
  }

  test("atomList") {
    val p = correctParser(atomList)_

    assert(p("(a b c)", List(TAtom("a"), TAtom("b"), TAtom("c"))))
  }

  test("list") {
    val p = correctParser(list)_

    assert(p("(\"test\" a b)", TList(List(TString("test"), TAtom("a"), TAtom("b")))))
    assert(p("(a b (c d (5 4)))",
      TList(List(TAtom("a"), TAtom("b"),
        TList(List(TAtom("c"), TAtom("d"),
          TList(List(TInt(5), TInt(4)))))))))
  }

  test("function") {
    val p = correctParser(function)_

    assert(p("(lambda (a b) 3)", TFunction(List(TAtom("a"), TAtom("b")), TInt(3))))
  }

  test("clasp") {
    val p = correctParser(clasp)_

    assert(p("'test", TQuote(TAtom("test"))))
    assert(p("test", TAtom("test")))
    assert(p("\"test\"", TString("test")))
    assert(p("5", TInt(5)))
    assert(p("5.0", TFloat(5)))
    assert(p("(lambda (a b) (a b c))", TFunction(List(TAtom("a"), TAtom("b")),
      TList(List(TAtom("a"), TAtom("b"), TAtom("c"))))))
    assert(p("(a \"b\" 5 5.0 ())", TList(List(TAtom("a"), TString("b"), TInt(5), TFloat(5), TList(Nil)))))
  }
}
