package com.crockeo.clasp

// Errors to do with language evaluation. Parse errors are handled by the parser
// themselves.
object ClaspError {
  sealed trait Kind

  case object SyntaxError extends Kind
  case object TypeError   extends Kind
}

class ClaspError(kind: ClaspError.Kind, ctx: String) extends Exception { }

// Evaluation of language constructs.
object Eval {
  import Language._

  type ClaspResult = Either[ClaspError, (Token, Context)]

  ////
  // The set of built-in functions.

  // Defining variables.
  private def builtin_def(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("def"), TAtom(name), v)) if (!builtIns.contains(name)) =>
      Eval(v, c) match {
        case Right((ev, ec)) => Right(ev, ec + (name -> ev))
        case Left(v)         => Left(v)
      }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: def"))
  }

  // Joining a list of elements (can be anything).
  private def builtin_add(t: Token, c: Context): ClaspResult = t match {
    case TList(TAtom("+") :: xs) =>
      ???

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: +"))
  }

  // Subtracting a list of numbers (left to right).
  private def builtin_sub(t: Token, c: Context): ClaspResult = t match {
    case TList(TAtom("-") :: xs) =>
      ???

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: -"))
  }

  // Multiplying a list of numbers.
  private def builtin_mul(t: Token, c: Context): ClaspResult = t match {
    case TList(TAtom("*") :: xs) =>
      ???

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: *"))
  }

  // Dividing a list of numbers (left to right).
  private def builtin_div(t: Token, c: Context): ClaspResult = t match {
    case TList(TAtom("/") :: xs) =>
      ???

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: /"))
  }

  // Indexing into a list or string.
  private def builtin_index(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("[]"), TInt(n), TList(l))) =>
      Right(l(n), c)

    case TList(List(TAtom("[]"), TInt(n), TString(s))) =>
      Right(TChar(s.charAt(n)), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: []"))
  }

  // Executing a quoted token.
  private def builtin_exec(t: Token, c: Context): ClaspResult = t match {
    case TQuote(qt) => Eval(qt, c)
    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: exec"))
  }

  // The set of built-in functions.
  private val builtIns: Map[String, (Token, Context) => ClaspResult] = Map(
    "def" -> builtin_def,
    "+"   -> builtin_add,
    "-"   -> builtin_sub,
    "*"   -> builtin_mul,
    "/"   -> builtin_div,
    "[]"  -> builtin_index,
    "exec" -> builtin_exec
  )

  ////
  // Applying a function.

  // Performing a function application.
  private def applyFn(t: Token, c: Context): ClaspResult = t match {
    case TList(TFunction(argNames, body) :: args) =>
      if (argNames.length != args.length)
        Left(new ClaspError(ClaspError.SyntaxError, s"Too ${if (argNames.length < args.length) "many" else "few"} arguments, expected ${argNames.length}, got ${args.length}."))
      else {
        val ec = argNames.zip(args).foldLeft(c.push)((c, as) => as match {
          case (TAtom(name), a) => c + (name -> a)
        })

        // TODO: One day make this less disgusting.
        body match {
          case TList(b) => b.foldLeft(Right(t, c): ClaspResult)((p, t) => p match {
            case Left(err)     => Left(err)
            case Right((_, c)) => Eval(t, c) match {
              case Left(err)     => Left(err)
              case Right((t, c)) => Right(t, c)
            }
          })

          case _        => Eval(body, ec)
        }
      }

    case _ => Left(new ClaspError(ClaspError.SyntaxError, "Malformed function application."))
  }

  ////
  // General evaluation.

  // Evaluating a token into its reduced form.
  def apply(t: Token, c: Context): Either[ClaspError, (Token, Context)] = t match {
    // Variable replacement.
    case TAtom(s) if (c.contains(s)) => Right((c(s), c))

    // Built-ins.
    case TList(TAtom(name) :: xs) if (builtIns.contains(name)) => builtIns(name)(t, c)

    // Function application.
    case TList(TFunction(_, _) :: xs) => applyFn(t, c)
    // TODO: Named function variables?

    case _ => Right((t, c))
  }
}
