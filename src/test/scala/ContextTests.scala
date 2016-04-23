package com.crockeo.clasp;

import org.scalatest._

class ContextTests extends FunSuite {
  import Language._

  test("default constructor") {
    assert(new Context() == new Context(List(Map(): Map[String, Token])))
  }

  test("push") {
    assert(new Context().push ==
      new Context(List(Map(), Map())))
    assert(new Context(Nil).push ==
      new Context(List(Map())))
  }

  test("pop") {
    assert(new Context().pop ==
      new Context())
    assert(new Context().push.pop == new Context())
    assert(new Context().push.push.pop == new Context().push)
    assert(new Context(Nil).pop == new Context())
  }

  test("add") {
    assert(new Context() + ("a" -> TAtom("lol")) ==
      new Context(List(Map("a" -> TAtom("lol")))))
  }

  test("apply") {
    assert((new Context() + ("a" -> TAtom("lol")))("a") ==
      TAtom("lol"))

    val lvl1 = new Context() + ("a" -> TAtom("lol"))
    val lvl2 = lvl1.push + ("a" -> TAtom("notlol"))

    assert(lvl1("a") == TAtom("lol"))
    assert(lvl2("a") == TAtom("notlol"))
    assert(lvl2.pop()("a") == TAtom("lol"))
  }
}
