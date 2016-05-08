package com.crockeo.clasp

// The declaration of all of the implicits for the project.
object Implicits {
  // Implementing Functor & Monad over an Either.
  implicit class EitherCollection[A, B](e: Either[A, B]) {
    def map[C](f: B => C): Either[A, C] = e match {
      case Left(a)  => Left(a)
      case Right(b) => Right(f(b))
    }

    def flatMap[C](f: B => Either[A, C]): Either[A, C] = e match {
      case Left(a) => Left(a)
      case Right(b) => f(b)
    }
  }
}
