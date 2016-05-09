package com.crockeo.clasp

import org.scalatest._

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
    assert(runStr("(= \"a\" \"a\")", empt) == Right(TBool(true), empt))
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

    assert(Eval(TList(List(TAtom("if"), TBool(true), TString("asdf"), TString("fdsa"))), empt) ==
      Right(TString("asdf"), empt))
    assert(File.runStr("(if (= 0 0) \"asdf\" \"fdsa\")", empt) == Right(TString("asdf"), empt))
  }
}
