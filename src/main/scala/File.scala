package com.crockeo.clasp

import java.io._

object File {
  import Result._

  // Running all of the tokens in a list.
  def runAll(ts: List[Token], c: Context): ClaspResult =
    ts.foldLeft(Right(Language.none, c): ClaspResult)((e, token) => e match {
      case Left(err)     => Left(err)
      case Right((_, c)) =>
        Eval(token, c)
    })

  // Parsing and running a string in a given context.
  def runStr(str: String, c: Context): ClaspResult = Language.parseSafe(str) match {
    case Left(err) => Left(err)
    case Right(ts) => runAll(ts, c)
  }

  // Running a singular file.
  def run(path: String, c: Context): ClaspResult =
    try {
      runStr(scala.io.Source.fromFile(path).mkString, c)
    } catch {
      case e: IOException => Left(new ClaspError(ClaspError.SystemError, "IOException in parsing one of the files."))
    }

  // Running a set of files.
  def run(paths: List[String], c: Context): ClaspResult = paths match {
    case Nil      => throw new IllegalArgumentException("Cannot run no files.")
    case x :: Nil => run(x, c)
    case x :: xs  => run(x, c) match {
      case Left(err) => Left(err)
      case Right((_, ec)) => run(xs, ec)
    }
  }
}
