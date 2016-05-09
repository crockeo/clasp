package com.crockeo.clasp

object Main {
  // The path to the prelude.
  private lazy val preludePath: String =
    sys.env("CLASPPATH") + "/prelude.clasp"
  private lazy val userPrelude: String =
    sys.env("CLASPPATH") + "/user-prelude.clasp"

  // Asking the user to configure their environment.
  def printConfigure(): Unit = {
    println("""clasp --
  * Before using clasp you must set the $CLASPPATH environment variable.""")
  }

  // Printing help for the program.
  def printHelp(): Unit = {
    println("""clasp --
  * clasp help                                  ; displays this page
  * clasp                                       ; opens the REPL
  * clasp <file1> [file2] ... [filen]           ; executing files from the
                                                  command line
  * clasp noprelude <file1> [file2] ... [filen] ; same as above, but without
                                                  loading the prelude or the
                                                  user prelude""")
  }

  // Starting the repl.
  def startRepl(withPrelude: Boolean): Unit = {
    println("|| clasp REPL ||")
    if (withPrelude) {
      File.run(List(preludePath, userPrelude), new Context()) match {
        case Left(err) =>
          println("Failed to load prelude or user prelude: " + err)

        case Right((_, c)) => REPL.start(c)
      }
    } else {
      REPL.start(new Context())
    }
  }

  // Parsing out a list of files.
  def parseFiles(paths: List[String]): Unit =
    File.run(paths, new Context())

  // The entry point to the application.
  def main(args: Array[String]): Unit = args.toList match {
    case _ if (sys.env.get("CLASPPATH") == None) => printConfigure
    case "noprelude" :: Nil                      => startRepl(false)
    case Nil                                     => startRepl(true)
    case "help" :: Nil                           => printHelp
    case "noprelude" :: x :: xs                  => parseFiles(x :: xs)
    case x :: xs                                 => parseFiles(preludePath :: userPrelude :: x :: xs)
    case _                                       => printHelp
  }
}
