package com.crockeo.clasp

import tools.jline.console.ConsoleReader
import scala.annotation.tailrec
import scala.io.StdIn
import Result._

object REPL {
  // Creating the console reader.
  private lazy val reader: ConsoleReader = new ConsoleReader()

  // Starting the REPL.
  def start(c: Context): Unit = {
    reader.setPrompt("> ")

    var line: String = reader.readLine
    var cc: Context = c
    while (line != "(q)" && line != "(quit)") {
      File.runStr(line, cc) match {
        case Left(err) => {
          reader.println(err.toString)
        }

        case Right((v, c)) => {
          reader.println(v.toString)
          cc = c
        }
      }

      line = reader.readLine
    }
  }
}
