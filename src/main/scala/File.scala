package com.crockeo.clasp

import java.io._

object File {
  // Running a singular file.
  def run(path: String, c: Context): Eval.ClaspResult = try {
    Language.parseSet(scala.io.Source.fromFile(path).mkString)
      .foldLeft(Right(Language.none, c): Eval.ClaspResult)((e, token) => e match {
        case Left(err) => Left(err)
        case Right((_, c)) =>
          Eval(token, c)
      })
  } catch {
    case e: IOException => Left(new ClaspError(
      ClaspError.SystemError, "IOException in parsing one of the files."))
  }

  // Running a set of files.
  def run(paths: List[String], c: Context): Eval.ClaspResult = paths match {
    case Nil      => throw new IllegalArgumentException("Cannot run no files.")
    case x :: Nil => run(x, c)
    case x :: xs  => run(x, c) match {
      case Left(err) => Left(err)
      case Right((_, ec)) => run(xs, ec)
    }
  }
}
