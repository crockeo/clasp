package com.crockeo.clasp

import org.scalatest._

class EvalTests extends FunSuite {
  import Language._

  // TODO: SO MANY FUCKING TESTS

  val emptyCtx = new Context()
  val withVars = emptyCtx + ("a" -> TInt(5)) + ("b" -> TFunction(List(TAtom("num")), TAtom("num")))

  test("basics") {
    assert(Eval(TQuote(TAtom("test")), emptyCtx) == Right(TQuote(TAtom("test")), emptyCtx))
    assert(Eval(TAtom("test"), emptyCtx) == Right(TAtom("test"), emptyCtx))
    assert(Eval(TChar('a'), emptyCtx) == Right(TChar('a'), emptyCtx))
    assert(Eval(TInt(5), emptyCtx) == Right(TInt(5), emptyCtx))
  }

  test("variable replacement") {
  }

  test("def") {
    assert(Eval(parse("(def a 5)"), emptyCtx) ==
      Right((TInt(5), emptyCtx + ("a" -> TInt(5)))), "def")
  }

  test("adding") {
  }

  test("subtracting") {
    assert(Eval(parse("(- 5 5 3)"), emptyCtx) ==
      Right(TInt(-3), emptyCtx), "- int")
    assert(Eval(parse("(- 5 2.5)"), emptyCtx) ==
      Right(TFloat(5 - 2.5f), emptyCtx), "- mixed")
  }

  test("multiplying") {
  }

  test("dividing") {
  }

  test("indexing") {
    assert(Eval(parse("([] 1 (a b c))"), emptyCtx) ==
      Right(TAtom("b"), emptyCtx), "index list")

    assert(Eval(parse("([] 1 \"testing\")"), emptyCtx) ==
      Right(TChar('e'), emptyCtx), "index string")
  }

  test("exec") {

  }

  test("tostr") {
  }

  test("print") {
  }

  test("function application") {
  }

  test("named function application") {
    // TODO: Implement the evaluator for this too.
  }


  test("builtins") {

    //assert(Eval(parse("(+ 5 5)"), emptyCtx) ==
      //Right(TInt(10), emptyCtx), "+ int")

    //assert(Eval(parse("(+ \"hm \" 5 ' ' 'a')"), emptyCtx) ==
      //Right(TString("hm 5 a"), emptyCtx), "+ mixed")


  }
}
