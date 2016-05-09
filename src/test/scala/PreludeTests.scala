package com.crockeo.clasp;

import org.scalatest._;

// Testing functionality within the prelude.
class PreludeTests extends FunSuite {
  import Result._

  val cp = sys.env("CLASPPATH")
  val prelude =
    File.run(List(s"$cp/prelude.clasp", s"$cp/user-prelude.clasp"), new Context()) match {
      case Left(err)     => throw new Exception(err)
      case Right((_, c)) => c
    }

  def eparse(s: String): ClaspResult =
    File.runStr(s, prelude)

  test("geq") {
    assert(eparse("(geq 1 2)") == Right(TBool(false), prelude))
    assert(eparse("(geq 2 2)") == Right(TBool(true), prelude))
    assert(eparse("(geq 3 2)") == Right(TBool(true), prelude))
  }

  test("leq") {
    assert(eparse("(leq 1 2)") == Right(TBool(true), prelude))
    assert(eparse("(leq 2 2)") == Right(TBool(true), prelude))
    assert(eparse("(leq 3 2)") == Right(TBool(false), prelude))
  }

  test("is-empty") {
    assert(eparse("(is-empty ())") == Right(TBool(true), prelude))
    assert(eparse("(is-empty (tail (1)))") == Right(TBool(true), prelude))
  }

  test("drop") {
    assert(eparse("(drop 0 (1 2 3))") == Right(TList(List(TInt(1), TInt(2), TInt(3))), prelude))
    assert(eparse("(drop 1 (1 2 3))") == Right(TList(List(TInt(2), TInt(3))), prelude))
  }

  test("take") {
    assert(eparse("(take 0 (1 2 3))") == Right(TList(Nil), prelude))
    assert(eparse("(take 2 (1 2 3))") == Right(TList(List(TInt(1), TInt(2))), prelude))
  }

  test("bool-switch") {
    assert(eparse("(bool-switch ((#f '5) (#t '2)))") == Right(TInt(2), prelude))
    assert(eparse("(bool-switch ((#t '5) (#t '2)))") == Right(TInt(5), prelude))
    assert(eparse("(bool-switch ((#f '5) (#f '5)))") == Right(TList(Nil), prelude))
  }

  test("switch") {
  }
}
