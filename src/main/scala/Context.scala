package com.crockeo.clasp;

import Language.Token

// A class to manage contexts.
class Context(private val cs: List[Map[String, Token]]) {
  // Constructing an empty Context (with a global map).
  def this() = this(List(Map()))

  // Equality between two Contexts.
  override def equals(o: Any): Boolean = o match {
    case c: Context => cs == c.cs
    case _          => false
  }

  // A flatted version of the context.
  private lazy val flatted: Map[String, Token] =
    cs.foldRight(Map(): Map[String, Token])((a, b) => b ++ a)

  // Adding an (empty) context map on top of the context.
  def push(): Context =
    new Context((Map(): Map[String, Token]) :: cs)

  // Popping a context off of the top. If the context only has the top-level
  // context then it returns the context that already exists.
  def pop(): Context = cs match {
    case Nil      => push
    case x :: Nil => this
    case x :: xs  => new Context(xs)
  }

  // Adding a new value to the most local context.
  def +(p: (String, Token)): Context = p match {
    case (k, v) => cs match {
      case Nil     => new Context(List(Map(k -> v)))
      case x :: xs => new Context((x + (k -> v)) :: xs)
    }
  }

  // Checking if the Context contains a value.
  def contains(s: String): Boolean =
    flatted.contains(s)

  // Accessing the values in this context.
  def apply(s: String): Token =
    flatted(s)
}
