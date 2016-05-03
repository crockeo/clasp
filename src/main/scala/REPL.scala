package com.crockeo.clasp

import scala.annotation.tailrec
import scala.io.StdIn
import Result._

object REPL {
  // Starting the REPL.
  @tailrec
  def start(c: Context): Unit = {
    print("> ")
    val line = StdIn.readLine

    if (line != "(q)" && line != "(quit)")
      File.runStr(line, c) match {
        case Left(err) => {
          println(err)
          start(c)
        }

        case Right((v, c)) => {
          println(v)
          start(c)
        }
      }

      //Language.parseSet(line) match {
        //case Nil => {
          //println("Failed to parse line: \"" + line + "\"")
          //start(c)
        //}

        //case l   =>
          //l.foldLeft(Right(Language.none, c): ClaspResult)((p, t) => p match {
            //case Left(err)     => Left(err)
            //case Right((_, c)) => Eval(t, c)
          //}) match {
            //case Left(err) => {
              //println(err)
              //start(c)
            //}

            //case Right((v, c)) => {
              //println(v)
              //start(c)
            //}
          //}
      //}
  }
}
