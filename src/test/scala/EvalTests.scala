package com.crockeo.clasp

import org.scalatest._

// Testing basic evaluation features:
//   * Most everything will evaluate to themselves without a context (except
//     built-ins, covered later.)
//
//   * An atom whose string is contained in a context will be replaced by that
//     value.
//
//   * Function application.
//
//   * Each element will be properly evaluated in a list (when not quoted). Aka
//     ((> 2 1) (= 2 1)) -> (#t #f)
class EvalTests extends FunSuite {
  import Language._
  import Result._

  val emptyCtx = new Context()
  val withVars = emptyCtx +
    ("a" -> TInt(5)) +
    ("b" -> TFunction(List(TAtom("num")), TAtom("num"))) +
    ("testfn" -> Language.parse("(lambda (n) (if (> n 0) #t #f))")) +
    ("recur" -> Language.parse("(lambda (n) (if (= n 1) 0 (+ 1 (recur (/ n 2)))))"))

  test("self eval") {
    assert(Eval(TQuote(TAtom("a")), emptyCtx) == Right(TQuote(TAtom("a")), emptyCtx))
    assert(Eval(TAtom("a"), emptyCtx) == Right(TAtom("a"), emptyCtx))
  }

  test("var replacement") {
    assert(Eval(parse("a"), withVars) == Right(TInt(5), withVars))
    assert(Eval(parse("b"), withVars) == Right(TFunction(List(TAtom("num")), TAtom("num")), withVars))
  }

  test("function application") {
    assert(Eval(parse("(b 20)"), withVars) == Right(TInt(20), withVars))
    assert(File.runStr("(testfn 0)", withVars) == Right(TBool(false), withVars))
    assert(File.runStr("(testfn 12)", withVars) == Right(TBool(true), withVars))
  }

  test("list eval") {
    assert(Eval(parse("((| #t #f) (& #t #f))"), emptyCtx) == Right(TList(List(TBool(true), TBool(false))), emptyCtx))
  }

  test("recursion") {
    assert(File.runStr("(recur 1)", withVars) == Right(TInt(0), withVars))
    assert(File.runStr("(recur 2)", withVars) == Right(TInt(1), withVars))
    assert(File.runStr("(recur 16)", withVars) == Right(TInt(4), withVars))
  }
}
