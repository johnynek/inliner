package com.github.johnynek.inliner

import scala.math.Equiv
import scala.util.{Try, Success, Failure}

/**
 * Equivalnce in testing (don't want to pull spire/algebra just for this)
 */
object Eqv {
  def eqv[T: Equiv, U](a: T, b: U)(implicit ev: U =:= T): Boolean = Equiv[T].equiv(a, ev(b))

  implicit def tryEquiv[T: Equiv]: Equiv[Try[T]] = new Equiv[Try[T]] {
    def equiv(a: Try[T], b: Try[T]) = (a, b) match {
      case (Success(l), Success(r)) => eqv(l, r)
      case (Failure(l), Failure(r)) => l.getMessage == r.getMessage
      case _ => false
    }
  }
}
