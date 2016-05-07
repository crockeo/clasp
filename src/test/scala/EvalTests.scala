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
    ("recur" -> Language.parse("(lambda (n) (if (= n 0) 0 (+ 1 (recur (/ n 2)))))"))

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
    assert(File.runStr("(recur 0)", withVars) == Right(TInt(0), withVars))
    assert(File.runStr("(recur 2)", withVars) == Right(TInt(1), withVars))
    assert(File.runStr("(recur 16)", withVars) == Right(TInt(4), withVars))
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
//   * "head" -> builtin_head,
//   * "tail" -> builtin_tail,
//   * "exec" -> builtin_exec,
//   * "tostr" -> builtin_tostr,
//   * "print" -> builtin_print,
//   * "if" -> builtin_if
class BuiltinTests extends FunSuite {
  import File.runStr
  import Language._
  import Result._

  // Checking if an Either is an error.
  def isErr[A, B](e: Either[A, B]): Boolean = e match {
    case Left(_) => true
    case _       => false
  }

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
    val fn = TFunction(List(TAtom("a")), TAtom("a"))

    assert(runStr("(defn test (a) a)", empt) ==
      Right(fn, empt + ("test" -> fn)))
  }

  test("builtin_add") {
    assert(runStr("(+ 1 1)", empt) == Right(TInt(2), empt))
    assert(runStr("(+ 1 1.0)", empt) == Right(TFloat(2.0f), empt))
    assert(runStr("(+ 1 \" \" 2)", empt) == Right(TString("1 2"), empt))
  }

  test("builtin_sub") {
    assert(runStr("(- 2 1)", empt) == Right(TInt(1), empt))
    assert(runStr("(- 2.0 1)", empt) == Right(TFloat(1.0f), empt))
  }

  test("builtin_mul") {
    // TODO
  }

  test("builtin_div") {
    // TODO
  }

  test("builtin_index") {
    assert(runStr("([] 0 (1 2 3))", empt) == Right(TInt(1), empt))
    assert(runStr("([] 3 (1 2 3))", empt) match {
      case Left(_) => true
      case _       => false
    })
  }

  test("builtin_len") {
    assert(Eval(parse("(len (a b c))"), empt) == Right(TInt(3), empt))
    assert(isErr(File.runStr("(len 5)", empt)))
  }

  test("builtin_head") {
    assert(File.runStr("(head (1 2 3))", empt) == Right(TInt(1), empt))
    assert(isErr(File.runStr("(head ())", empt)))
  }

  test("builtin_tail") {
    assert(File.runStr("(tail (1 2 3))", empt) == Right(TList(List(TInt(2), TInt(3))), empt))
    assert(isErr(File.runStr("(tail ())", empt)))
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
    assert(Eval(parse("(if (= 0 0) a b)"), empt) == Right(TAtom("a"), empt))
    assert(Eval(parse("(if #f a b)"), empt) == Right(TAtom("b"), empt))
    assert(Eval(parse("(if (= 0 1) a b)"), empt) == Right(TAtom("b"), empt))
  }
}
