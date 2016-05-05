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
  val withVars = emptyCtx + ("a" -> TInt(5)) + ("b" -> TFunction(List(TAtom("num")), TAtom("num")))

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
  }

  test("list eval") {
    assert(Eval(parse("((| #t #f) (& #t #f))"), emptyCtx) == Right(TList(List(TBool(true), TBool(false))), emptyCtx))
  }
}

// Testing the built-in functionality of the language.
//   * "=" -> builtin_eq,
//   * "not" -> builtin_not,
//   * "|" -> builtin_or,
//   * "&" -> builtin_and,
//   * "^" -> builtin_xor,
//   * "def" -> builtin_def,
//   * "defn" -> builtin_defn,
//   * "+"   -> builtin_add,
//   * "-"   -> builtin_sub,
//   * "*"   -> builtin_mul,
//   * "/"   -> builtin_div,
//   * "[]"  -> builtin_index,
//   * "len" -> builtin_len,
//   * "exec" -> builtin_exec,
//   * "tostr" -> builtin_tostr,
//   * "print" -> builtin_print,
//   * "if" -> builtin_if
class BuiltinTests extends FunSuite {
  import File.runStr
  import Language._
  import Result._

  val empt = new Context()

  test("builtin_eq") {
    assert(runStr("(= 1 2)", empt) == Right(TBool(false), empt))
  }

  test("builtin_gt") {
    assert(runStr("(> 1 2)", empt) == Right(TBool(false), empt))
    assert(runStr("(> 2 2)", empt) == Right(TBool(false), empt))
    assert(runStr("(> 3 2)", empt) == Right(TBool(true), empt))
  }

  test("builtin_lt") {
    assert(runStr("(< 1 2)", empt) == Right(TBool(true), empt))
    assert(runStr("(< 2 2)", empt) == Right(TBool(false), empt))
    assert(runStr("(< 3 2)", empt) == Right(TBool(false), empt))
  }

  test("builtin_not") {
    assert(runStr("(! #t)", empt) == Right(TBool(false), empt))
    assert(runStr("(! #f)", empt) == Right(TBool(true), empt))
  }

  test("builtin_or") {
    assert(runStr("(| #f #f)", empt) == Right(TBool(false), empt))
    assert(runStr("(| #f #t)", empt) == Right(TBool(true), empt))
    assert(runStr("(| #t #f)", empt) == Right(TBool(true), empt))
    assert(runStr("(| #t #t)", empt) == Right(TBool(true), empt))
  }

  test("builtin_and") {
    assert(runStr("(& #f #f)", empt) == Right(TBool(false), empt))
    assert(runStr("(& #f #t)", empt) == Right(TBool(false), empt))
    assert(runStr("(& #t #f)", empt) == Right(TBool(false), empt))
    assert(runStr("(& #t #t)", empt) == Right(TBool(true), empt))
  }

  test("builtin_xor") {
    assert(runStr("(^ #f #f)", empt) == Right(TBool(false), empt))
    assert(runStr("(^ #f #t)", empt) == Right(TBool(true), empt))
    assert(runStr("(^ #t #f)", empt) == Right(TBool(true), empt))
    assert(runStr("(^ #t #t)", empt) == Right(TBool(false), empt))
  }

  test("builtin_def") {
    assert(runStr("(def a b)", empt) == Right(TAtom("b"), empt + ("a" -> TAtom("b"))))
  }

  test("builtin_defn") {

  }

  test("builtin_add") {

  }

  test("builtin_sub") {

  }

  test("builtin_mul") {

  }

  test("builtin_div") {

  }

  test("builtin_index") {

  }

  test("builtin_len") {
    assert(Eval(parse("(len (a b c))"), empt) == Right(TInt(3), empt))
  }

  test("builtin_exec") {
    assert(Eval(parse("(exec '(+ 1 1))"), empt) == Right(TInt(2), empt))
    assert(Eval(parse("(exec '(if #t (+ 1 1) 0))"), empt) == Right(TInt(2), empt))
  }

  test("builtin_tostr") {
    assert(Eval(parse("(tostr 1)"), empt) == Right(TString("1"), empt))
    assert(Eval(parse("(tostr a)"), empt) == Right(TString("a"), empt))
  }

  test("builtin_print") {
    // TODO: I can't actually test this properly.
  }

  test("builtin_if") {
    assert(Eval(parse("(if #t a b)"), empt) == Right(TAtom("a"), empt))
    assert(Eval(parse("(if #f a b)"), empt) == Right(TAtom("b"), empt))
  }
}
