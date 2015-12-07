package com.github.johnynek

import scala.language.experimental.macros
import inliner.MacroCompat.{Context, newTerm, recurse}

package object inliner {

  /**
   * Identity map functions come up in for blocks when you yield the last result
   */
  private[inliner] def isIdentityFn[T, U](c: Context)(fn: c.Expr[T => U]): Boolean = {
    import c.universe._
    fn.tree match {
      case Function(ValDef(_, argName, _, EmptyTree) :: Nil, Ident(res)) => // literal function definition
        argName == res
      case other =>
        false
    }
  }

  private[inliner] def function1Apply[T, U](c: Context)(fn: c.Expr[T => U]): (c.Tree, c.Tree) = {
    import c.universe._
    // There are only a few possibilities for expressions of type T => U
    def go(t: c.Tree): (c.Tree, c.Tree) = t match {
      case Block(exprs, result) =>
        val (arg, tree) = go(result)
        (arg, Block(exprs.map(e => recurse(c)(e)), tree))
      case Function(ValDef(_, argName, _, EmptyTree) :: Nil, tree) => // literal function definition
        (Ident(argName), recurse(c)(tree))
      case other =>
        c.abort(other.pos, s"requires a literal function, was passed: $other")
        /*
      case Ident(fnName) => // passing an existing named function
        val arg = newTerm(c, "arg")
        (Ident(arg), Apply(Ident(fnName), List(Ident(arg))))
      case Select(from, attr) =>
        // this is a function that is a member
        val arg = newTerm(c, "arg")
        (Ident(arg), Apply(Select(from, attr), List(Ident(arg))))
      case other =>
        c.abort(other.pos, s"unsupported fn: $other")
        */
    }
    go(fn.tree)
  }
  private[inliner] def function2Apply[T, U, V](c: Context)(fn: c.Expr[(T, U) => V]): (c.TermName, c.TermName, c.Tree) = {
    import c.universe._

    // There are only a few possibilities for expressions of type T => U
    def go(t: c.Tree): (c.TermName, c.TermName, c.Tree) = t match {
      case Block(exprs, result) =>
        val (argT, argU, tree) = go(result)
        (argT, argU, Block(exprs.map(e => recurse(c)(e)), tree))
      case Function(ValDef(_, argT, _, EmptyTree) :: ValDef(_, argU, _, EmptyTree) :: Nil, tree) =>
        // literal function definition
        (argT, argU, recurse(c)(tree))
      case other =>
        c.abort(other.pos, s"requires a literal function, was passed: $other")
      /*
      case Ident(fnName) => // passing an existing named function
        val argT = newTerm(c, "argT")
        val argU = newTerm(c, "argU")
        (argT, argU, Apply(Ident(fnName), List(Ident(argT), Ident(argU))))
      case Select(from, attr) =>
        // this is a function that is a member
        val argT = newTerm(c, "argT")
        val argU = newTerm(c, "argU")
        (argT, argU, Apply(Select(from, attr), List(Ident(argT), Ident(argU))))
      case other =>
        c.abort(other.pos, s"unsupported fn: $other")
        */
    }
    go(fn.tree)
  }
}
