package com.crockeo.clasp

// Evaluation of language constructs.
object Eval {
  import Language._
  import Result._

  ////
  // Helper functions.

  // Reducing a list of tokens down into a single token by some passed function.
  private def reduceList(t: List[Token], c: Context)(fn: (Token, Token) => ClaspResult): ClaspResult = t match {
    case Nil          => Left(new ClaspError(ClaspError.ValueError, "Error: You cannot reduce an empty list."))
    case x :: Nil     => Right((x, c))
    case x :: y :: xs => for {
      ex <- Eval(x, c)
      ey <- Eval(y, ex._2)
      fd <- fn(ex._1, ey._1)
      v  <- reduceList(fd._1 :: xs, fd._2)(fn)
    } yield v
  }

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

  // Checking the equality between 2 values.
  private def builtin_eq(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("="), t1, t2)) =>
      Right(TBool(t1 == t2), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: eq"))
  }

  // Negating a boolean or an integer.
  private def builtin_not(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("!"), t)) => t match {
      case TBool(b) => Right(TBool(!b), c)
      case TInt(n) => Right(TInt(~n), c)

      case _ =>
        Left(new ClaspError(ClaspError.SyntaxError,
          "Invalid call to builtin: not; you can only negate booleans and integers."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: not"))
  }

  // AND-ing together 2 booleans 2 integers.
  private def builtin_and(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("&"), t1, t2)) => (t1, t2) match {
      case (TBool(b1), TBool(b2)) => Right(TBool(b1 && b2), c)
      case (TInt(n1), TInt(n2)) => Right(TInt(n1 & n2), c)

      case _ =>
        Left(new ClaspError(ClaspError.SyntaxError,
          "Invalid call to builtin: and; you can only AND booleans and integers."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: and"))
  }

  // OR-ing together 2 booleans or 2 integers.
  private def builtin_or(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("|"), t1, t2)) => (t1, t2) match {
      case (TBool(b1), TBool(b2)) => Right(TBool(b1 || b2), c)
      case (TInt(n1), TInt(n2)) => Right(TInt(n1 | n2), c)

      case _ =>
        Left(new ClaspError(ClaspError.SyntaxError,
          "Invalid call to builtin: and; you can only OR booleans and integers."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: or"))
  }

  // XOR-ing together 2 booleans or 2 integers.
  private def builtin_xor(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("^"), t1, t2)) => (t1, t2) match {
      case (TBool(b1), TBool(b2)) => Right(TBool(b1 ^ b2), c)
      case (TInt(n1), TInt(n2)) => Right(TInt(n1 ^ n2), c)

      case _ =>
        Left(new ClaspError(ClaspError.SyntaxError,
          "Invalid call to builtin: and; you can only XOR booleans and integers."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: xor"))
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
      reduceList(xs, c)((a, b) => (a, b) match {
        case (TInt(x),   TInt(y))   => Right(TInt(x - y), c)
        case (TFloat(x), TInt(y))   => Right(TFloat(x - y), c)
        case (TInt(x),   TFloat(y)) => Right(TFloat(x - y), c)
        case (TFloat(x), TFloat(y)) => Right(TFloat(x - y), c)

        case _ =>
          Left(new ClaspError(ClaspError.TypeError, "Cannot substract anything but number types."))
      })

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: -"))
  }

  // Multiplying a list of numbers.
  private def builtin_mul(t: Token, c: Context): ClaspResult = t match {
    case TList(TAtom("*") :: xs) =>
      reduceList(xs, c)((a, b) => (a, b) match {
        case (TInt(x),   TInt(y))   => Right(TInt(x * y), c)
        case (TFloat(x), TInt(y))   => Right(TFloat(x * y), c)
        case (TInt(x),   TFloat(y)) => Right(TFloat(x * y), c)
        case (TFloat(x), TFloat(y)) => Right(TFloat(x * y), c)

        case _ =>
          Left(new ClaspError(ClaspError.TypeError, "Cannot multiply anything but number types."))
      })

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: *"))
  }

  // Dividing a list of numbers (left to right).
  private def builtin_div(t: Token, c: Context): ClaspResult = t match {
    case TList(TAtom("*") :: xs) =>
      reduceList(xs, c)((a, b) => (a, b) match {
        case (TInt(x),   TInt(y))   => Right(TInt(x / y), c)
        case (TFloat(x), TInt(y))   => Right(TFloat(x / y), c)
        case (TInt(x),   TFloat(y)) => Right(TFloat(x / y), c)
        case (TFloat(x), TFloat(y)) => Right(TFloat(x / y), c)

        case _ =>
          Left(new ClaspError(ClaspError.TypeError, "Cannot multiply anything but number types."))
      })

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
    case TList(List(TAtom("exec"), TQuote(qt))) => Eval(qt, c)
    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: exec"))
  }

  // Converting a token to its string representation.
  private def builtin_tostr(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("tostr"), t)) =>
      Right(TString(t.toString), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: exec"))
  }

  // Printing out to the console.
  private def builtin_print(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("print"), TQuote(TAtom("noline")), t)) => {
      print(t)
      Right(Language.none, c)
    }

    case TList(List(TAtom("print"), t)) => {
      println(t)
      Right(Language.none, c)
    }

    // TODO: Printing multiple values at a time.

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: print"))
  }

  // If/else blocks.
  private def builtin_if(t: Token, c: Context): ClaspResult = t match {
    case TList(List(TAtom("if"), TBool(b), t, f)) =>
      if (b) Eval(t, c)
      else   Eval(f, c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: if"))
  }

  // The set of built-in functions.
  private val builtIns: Map[String, (Token, Context) => ClaspResult] = Map(
    "=" -> builtin_eq,
    "not" -> builtin_not,
    "|" -> builtin_or,
    "&" -> builtin_and,
    "^" -> builtin_xor,
    "def" -> builtin_def,
    "+"   -> builtin_add,
    "-"   -> builtin_sub,
    "*"   -> builtin_mul,
    "/"   -> builtin_div,
    "[]"  -> builtin_index,
    "exec" -> builtin_exec,
    "tostr" -> builtin_tostr,
    "print" -> builtin_print,
    "if" -> builtin_if
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

  // Evaluating a list of arguments.
  def evalList(l: List[Token], c: Context): ClaspResult =
    l.foldRight(Right((TList(Nil), c)): ClaspResult)((v, p) => p match {
      case Left(err)             => Left(err)
      case Right((TList(xs), c)) => Eval(v, c) match {
        case Left(err)     => Left(err)
        case Right((v, c)) => Right(TList(v :: xs), c)
      }

      case _                     =>
        Left(new ClaspError(ClaspError.SyntaxError, "Malformed list evaluation - I don't know how this happened."))
    })

  // Evaluating a token into its reduced form.
  def apply(t: Token, c: Context): ClaspResult = t match {
    // Variable replacement.
    case TAtom(s) if (c.contains(s)) => Right((c(s), c))

    // Working with lists.
    case TList(l) => evalList(l, c) match {
      case Left(err) => Left(err)
      case Right(t)  => t._1 match {
        case TList(TAtom(name) :: xs) if (builtIns.contains(name)) => builtIns(name)(t._1, t._2)
        case TList(TFunction(_, _) :: xs)                          => applyFn(t._1, t._2)
        case _                                                     => Right(t)
      }
    }

    // Not evaluating everything else.
    case _ => Right((t, c))
  }
}
