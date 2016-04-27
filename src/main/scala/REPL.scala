package com.crockeo.clasp

import scala.annotation.tailrec
import scala.io.StdIn

object REPL {
  // Starting the REPL.
  @tailrec
  def start(c: Context): Unit = {
    print("> ")
    val line = StdIn.readLine

    if (line != "(q)" && line != "(quit)") {
      Eval(Language.parse(line), c) match {
        case Left(err)   => println(err)
        case Right((v, c)) =>
          println(v)
          start(c)
      }
    }
  }
}
