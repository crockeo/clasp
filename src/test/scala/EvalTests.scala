package com.crockeo.clasp

import org.scalatest._

// Testing language evaluation.
class EvalTests extends FunSuite {
  import Language._
  import Eval._

  val globalCtx: List[Context] = List(Map())
  val withVariable: List[Context] = List(Map("testing" -> TInt(5)))

  test("quote") {
    assert(eval(parse("'testing"), globalCtx) == (TQuote("testing"), globalCtx))
    assert(eval(TQuote("testing"), globalCtx) == (TQuote("testing"), globalCtx))
    assert(eval(TQuote("testing"), withVariable) == (TQuote("testing"), withVariable))
  }

  test("atom") {
    assert(eval(TAtom("testing"), globalCtx) == (TAtom("testing"), globalCtx))
    assert(eval(TAtom("testing"), withVariable) == (TInt(5), withVariable))
  }

  test("string") {
  }

  test("int") {

  }

  test("float") {

  }

  test("list") {

  }

  test("function") {

  }

  test("builtin - def") {
    assert(eval(parse("(def testing 5)"), globalCtx) == (TInt(5), withVariable))
  }

  test("builtin - +") {
    assert(eval(parse("(+ 5 5)"), globalCtx) == (TInt(10), globalCtx))
    assert(eval(parse("(+ 5.0 5)"), globalCtx) == (TFloat(10), globalCtx))
    assert(eval(parse("(+ 5 5.0)"), globalCtx) == (TFloat(10), globalCtx))
    assert(eval(parse("(+ 5.0 5.0)"), globalCtx) == (TFloat(10), globalCtx))
    assert(eval(parse("(+ \"te\" \"st\")"), globalCtx) == (TString("test"), globalCtx))
  }

  test("builtin - -") {

  }

  test("builtin - *") {

  }

  test("builtin - /") {

  }
}
