package com.crockeo.clasp

// Evaluation of language constructs.
object Eval {
  import Language._

  // A mapping of variable names (through TAtoms, as they are referenced in the
  // language) to values.
  type Context = Map[String, Token]

  // Flatting a set of contexts such that the highest level context is at the
  // end of the list, and the most local context is at the front. 
  def flattenContexts(cs: List[Context]): Context = cs match {
    case Nil     => Map()
    case x :: xs => flattenContexts(xs) ++ x
  }

  // Adding a value to the top-level context.
  def add(cs: List[Context], s: String, v: Token): List[Context] = cs match {
    case Nil     => Nil
    case x :: xs => x + (s -> v) :: xs
  }

  // Evaluating a token into its reduced form.
  def eval(t: Token, cs: List[Context]): (Token, List[Context]) = {
    val c = flattenContexts(cs)

    t match {
      case TAtom(s) if (c.contains(s))               => (c(s), cs)
      case TList(List(TAtom("def"), TAtom(name), v)) => (v, add(cs, name, v))
      case _                                         => (t, cs)
    }
  }
}
