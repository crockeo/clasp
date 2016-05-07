package com.crockeo.clasp

// The set of built-in functionality that exists in the language.
object Builtin {
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
  private def builtin_def(t: List[Token], c: Context): ClaspResult = t match {
    case List(TAtom(name), v) =>
      if (Builtin.contains(name))
        Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin def; cannot redefine builtin values."))
      else
        Eval(v, c) match {
          case Right((ev, ec)) => Right(ev, ec + (name -> ev))
          case Left(v)         => Left(v)
        }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: def; invalid syntax."))
  }

  // Defining functions.
  private def builtin_defn(t: List[Token], c: Context): ClaspResult = t match {
    case List(TAtom(name), TList(args), body) =>
      if (Builtin.contains(name))
        Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: defn; cannot redefine builtin functions."))
      else
        Eval(Language.parse(s"(def $name (lambda ${TList(args)} $body))"), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: defn; invalid syntax."))
  }

  // Checking the equality between 2 values.
  private def builtin_eq(t: List[Token], c: Context): ClaspResult = t match {
    case List(t1, t2) =>
      Right(TBool(t1 == t2), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: eq"))
  }

  // Checking if an item is greater than another.
  private def builtin_gt(t: List[Token], c: Context): ClaspResult = t match {
    case List(t1, t2) => (t1, t2) match {
      case (TInt(a),   TInt(b))     => Right(TBool(a > b), c)
      case (TInt(a),   TFloat(b))   => Right(TBool(a > b), c)
      case (TFloat(a), TInt(b))     => Right(TBool(a > b), c)
      case (TFloat(a), TFloat(b))   => Right(TBool(a > b), c)
      case (TString(a), TString(b)) => Right(TBool(a > b), c)

      case _ =>
        Left(new ClaspError(ClaspError.ValueError, "Invalid call to builtin: gt; mismatched types."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: gt"))
  }

  // Checking if an item is less than another.
  private def builtin_lt(t: List[Token], c: Context): ClaspResult = t match {
    case List(t1, t2) => (t1, t2) match {
      case (TInt(a),   TInt(b))     => Right(TBool(a < b), c)
      case (TInt(a),   TFloat(b))   => Right(TBool(a < b), c)
      case (TFloat(a), TInt(b))     => Right(TBool(a < b), c)
      case (TFloat(a), TFloat(b))   => Right(TBool(a < b), c)
      case (TString(a), TString(b)) => Right(TBool(a < b), c)

      case _ =>
        Left(new ClaspError(ClaspError.ValueError, "Invalid call to builtin lt; mismatched types."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: lt"))
  }

  // Negating a boolean or an integer.
  private def builtin_not(t: List[Token], c: Context): ClaspResult = t match {
    case List(t) => t match {
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
  private def builtin_and(t: List[Token], c: Context): ClaspResult = t match {
    case List(t1, t2) => (t1, t2) match {
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
  private def builtin_or(t: List[Token], c: Context): ClaspResult = t match {
    case List(t1, t2) => (t1, t2) match {
      case (TBool(b1), TBool(b2)) => Right(TBool(b1 || b2), c)
      case (TInt(n1), TInt(n2)) => Right(TInt(n1 | n2), c)

      case _ =>
        Left(new ClaspError(ClaspError.SyntaxError,
          "Invalid call to builtin: or; you can only OR booleans and integers."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: or"))
  }

  // XOR-ing together 2 booleans or 2 integers.
  private def builtin_xor(t: List[Token], c: Context): ClaspResult = t match {
    case List(t1, t2) => (t1, t2) match {
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
  private def builtin_add(t: List[Token], c: Context): ClaspResult =
    reduceList(t, c)((a, b) => (a, b) match {
      case (TList(ts1), TList(ts2)) => Right(TList(ts1 ++ ts2), c)
      case (t, TList(ts))           => Right(TList(t :: ts), c)
      case (TList(ts), t)           => Right(TList(ts ++ List(t)), c)

      case (TString(s), t) => Right(TString(s + t.toString), c)
      case (t, TString(s)) => Right(TString(t.toString + s), c)

      case (TInt(x),   TInt(y))   => Right(TInt(x + y), c)
      case (TFloat(x), TInt(y))   => Right(TFloat(x + y), c)
      case (TInt(x),   TFloat(y)) => Right(TFloat(x + y), c)
      case (TFloat(x), TFloat(y)) => Right(TFloat(x + y), c)

      case _ =>
        Left(new ClaspError(ClaspError.TypeError,
          s"Failed to join '$a' and '$b'."))
    })

  // Subtracting a list of numbers (left to right).
  private def builtin_sub(t: List[Token], c: Context): ClaspResult =
    reduceList(t, c)((a, b) => (a, b) match {
      case (TInt(x),   TInt(y))   => Right(TInt(x - y), c)
      case (TFloat(x), TInt(y))   => Right(TFloat(x - y), c)
      case (TInt(x),   TFloat(y)) => Right(TFloat(x - y), c)
      case (TFloat(x), TFloat(y)) => Right(TFloat(x - y), c)

      case _ =>
        Left(new ClaspError(ClaspError.TypeError, "Cannot substract anything but number types."))
    })

  // Multiplying a list of numbers.
  private def builtin_mul(t: List[Token], c: Context): ClaspResult =
    reduceList(t, c)((a, b) => (a, b) match {
      case (TInt(x),   TInt(y))   => Right(TInt(x * y), c)
      case (TFloat(x), TInt(y))   => Right(TFloat(x * y), c)
      case (TInt(x),   TFloat(y)) => Right(TFloat(x * y), c)
      case (TFloat(x), TFloat(y)) => Right(TFloat(x * y), c)

      case _ =>
        Left(new ClaspError(ClaspError.TypeError, "Cannot multiply anything but number types."))
    })

  // Dividing a list of numbers (left to right).
  private def builtin_div(t: List[Token], c: Context): ClaspResult =
    reduceList(t, c)((a, b) => (a, b) match {
      case (TInt(x),   TInt(y))   => Right(TInt(x / y), c)
      case (TFloat(x), TInt(y))   => Right(TFloat(x / y), c)
      case (TInt(x),   TFloat(y)) => Right(TFloat(x / y), c)
      case (TFloat(x), TFloat(y)) => Right(TFloat(x / y), c)

      case _ =>
        Left(new ClaspError(ClaspError.TypeError, "Cannot multiply anything but number types."))
    })

  // Indexing into a list or string.
  private def builtin_index(t: List[Token], c: Context): ClaspResult = t match {
    case List(TInt(n), TList(l)) if (n < l.length) =>
      Right(l(n), c)

    case List(TInt(n), TString(s)) if (n < s.length) =>
      Right(TChar(s.charAt(n)), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: []"))
  }

  // Finding the length of a string or a list.
  private def builtin_len(t: List[Token], c: Context): ClaspResult = t match {
    case List(t) => t match {
      case TList(l)   => Right(TInt(l.length), c)
      case TString(s) => Right(TInt(s.length), c)

      case _ =>
        Left(new ClaspError(ClaspError.TypeError, "Invalid call to builtin: len; you can only take the length of lists and strings."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: len"))
  }

  // Finding the head of a list.
  private def builtin_head(t: List[Token], c: Context): ClaspResult = t match {
    case List(TList(l)) => l match {
      case x :: _ => 
        Right(x, c)

      case Nil =>
        Left(new ClaspError(ClaspError.ValueError, "Invalid call to builtin: head; head on empty list."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: head"))
  }

  // Finding the tail of a list.
  private def builtin_tail(t: List[Token], c: Context): ClaspResult = t match {
    case List(TList(l)) => l match {
      case _ :: xs =>
        Right(TList(xs), c)

      case Nil =>
        Left(new ClaspError(ClaspError.ValueError, "Invalid call to builtin: tail; tail on empty list."))
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: tail"))
  }

  // Executing a quoted token.
  private def builtin_exec(t: List[Token], c: Context): ClaspResult = t match {
    case List(TQuote(qt)) => Eval(qt, c)
    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: exec"))
  }

  // Converting a token to its string representation.
  private def builtin_tostr(t: List[Token], c: Context): ClaspResult = t match {
    case List(t) =>
      Right(TString(t.toString), c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: exec"))
  }

  // Printing out to the console.
  private def builtin_print(t: List[Token], c: Context): ClaspResult = t match {
    case List(TQuote(TAtom("noline")), t) => {
      print(t)
      Right(Language.none, c)
    }

    case List(t) => {
      println(t)
      Right(Language.none, c)
    }

    case TQuote(TAtom("noline")) :: xs if (xs != Nil) => {
      print(xs.map(_.toString).reduceLeft(_ + " " + _))
      Right(Language.none, c)
    }

    case xs if (xs != Nil) => {
      println(xs.map(_.toString).reduceLeft(_ + " " + _))
      Right(Language.none, c)
    }

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: print"))
  }

  // If/else blocks.
  private def builtin_if(t: List[Token], c: Context): ClaspResult = t match {
    case List(TBool(b), t, f) =>
      if (b) Eval(t, c)
      else   Eval(f, c)

    case _ =>
      Left(new ClaspError(ClaspError.SyntaxError, "Invalid call to builtin: if"))
  }

  // The set of built-in functions.
  private val builtins: Map[String, (List[Token], Context) => ClaspResult] = Map(
    "=" -> builtin_eq,
    ">" -> builtin_gt,
    "<" -> builtin_lt,
    "!" -> builtin_not,
    "|" -> builtin_or,
    "&" -> builtin_and,
    "^" -> builtin_xor,
    "def" -> builtin_def,
    "defn" -> builtin_defn,
    "+"   -> builtin_add,
    "-"   -> builtin_sub,
    "*"   -> builtin_mul,
    "/"   -> builtin_div,
    "[]"  -> builtin_index,
    "len" -> builtin_len,
    "head" -> builtin_head,
    "tail" -> builtin_tail,
    "exec" -> builtin_exec,
    "tostr" -> builtin_tostr,
    "print" -> builtin_print,
    "if" -> builtin_if
  )

  // Checking if a builtin exists.
  def contains(s: String): Boolean =
    builtins.contains(s)

  // Getting a builtin.
  def apply(s: String): (List[Token], Context) => ClaspResult =
    builtins(s)
}
