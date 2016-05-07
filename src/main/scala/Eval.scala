package com.crockeo.clasp

// Evaluation of language constructs.
object Eval {
  import Language._
  import Result._

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

        // TODO: FINISH HIM
        (body match {
          // Executing built-ins.
          case TList(TAtom(name) :: xs) if (Builtin.contains(name)) => for {
            elist <- evalList(xs, ec)
            bi <- Builtin(name)(elist._1, elist._2)
          } yield bi

          // Executing functions.
          case TList(TFunction(args, body) :: xs) => for {
            elist <- evalList(xs, ec)// TODO
            afn <- applyFn(TList(TFunction(args, body) :: elist._1), elist._2)
          } yield (afn._1, c)

          // Executing lists of commands.
          case TList(b) => b.foldLeft(Right(t, ec): ClaspResult)((p, t) => p match {
            case Left(err)     => Left(err)
            case Right((_, c)) => Eval(t, c) match {
              case Left(err)     => Left(err)
              case Right((t, c)) => Right(t, c)
            }
          })

          // Executing anything else.
          case _        => Eval(body, ec)
        }) match {
          case Left(err)   => Left(err)
          case Right((t, _)) => Right(t, c) // Escaping out of the context.
        }
      }

    case _ => Left(new ClaspError(ClaspError.SyntaxError, "Malformed function application."))
  }

  ////
  // General evaluation.

  // Evaluating a list of arguments.
  def evalList(l: List[Token], c: Context): Either[ClaspError, (List[Token], Context)] =
    l.foldRight(Right(List(), c): Either[ClaspError, (List[Token], Context)])((v, ep) => for {
      p <- ep
      e <- Eval(v, p._2)
    } yield (e._1 :: p._1, e._2))

  // Evaluating a token into its reduced form.
  def apply(t: Token, c: Context): ClaspResult = t match {
    // Variable replacement.
    case TAtom(s) if (c.contains(s)) => Right((c(s), c))

    // We have to single out defn out of list application because it can't have
    // the rest of the arguments be applied yet.
    case TList(TAtom("defn") :: xs) =>
      Builtin("defn")(xs, c)

    // Working with lists.
    case TList(l) => evalList(l, c) match {
      case Left(err) => Left(err)
      case Right(t)  => t._1 match {
        case TAtom(name) :: xs if (Builtin.contains(name)) => Builtin(name)(xs, t._2)
        case TFunction(_, _) :: xs                         => applyFn(TList(t._1), t._2)
        case _                                             => Right(TList(t._1), t._2)
      }
    }

    // Not evaluating everything else.
    case _ => Right((t, c))
  }
}
