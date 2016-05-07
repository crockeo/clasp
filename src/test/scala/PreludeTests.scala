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

  test("bool-switch") {
    assert(eparse("(bool-switch ((#f '5) (#t '2)))") == Right(TInt(2), prelude))
    assert(eparse("(bool-switch ((#t '5) (#t '2)))") == Right(TInt(5), prelude))
  }

  test("switch") {
  }
}
