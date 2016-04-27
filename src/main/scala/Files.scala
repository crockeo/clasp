package com.crockeo.clasp

object File {
  // Running a singular file.
  def run(path: String, c: Context): Eval.ClaspResult =
    Eval(Language.parse(scala.io.Source.fromFile(path).mkString), c)

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
