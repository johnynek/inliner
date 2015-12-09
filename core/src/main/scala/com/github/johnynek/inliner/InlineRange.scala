package com.github.johnynek.inliner

import MacroCompat.{Context, newTerm, recurse, singleConsArg}
import scala.collection.immutable.Range
import scala.language.experimental.macros
/**
 * This class is never allocated because all the macros require
 * them to be called as (new InlineOption(o).method) or it won't compile,
 * if it compiles, that bit is replaced with code using if/else
 */
class InlineRange(o: Range) {
  import InlineRange._

  def find(fn: Int => Boolean): Option[Int] = macro findMethod
  def forall(fn: Int => Boolean): Boolean = macro forallMethod
  def foldLeft[U](init: U)(fn: (U, Int) => U): U = macro foldLeftMethod[U]
  def foreach(fn: Int => Unit): Unit = macro foreachMethod
  def reduceOption(fn: (Int, Int) => Int): Option[Int] = macro reduceOptionMethod
}
object InlineRange {
  implicit class ToInlineRange(val o: Range) extends AnyVal {
    @inline def inline: InlineRange = new InlineRange(o)
  }

  def find(ts: Range)(fn: Int => Boolean): Option[Int] = macro findMacro

  private[this] def findTree(c: Context)(ts: c.Tree, fn: c.Expr[Int => Boolean]): c.Expr[Option[Int]] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val end = newTerm(c, "end")
    val pos = newTerm(c, "pos")
    val step = newTerm(c, "step")
    val res = newTerm(c, "res")
    val rng = newTerm(c, "range")
    val tree = q"""{
      val $rng = $ts
      var $res: _root_.scala.Option[Int] = _root_.scala.None
      var $pos = $rng.start
      val $end = $rng.end
      val $step = $rng.step
      while($pos < $end) {
        val $arg = $pos
        if($newTree) { $res = _root_.scala.Some($arg); $pos = $end }
        $pos += $step
      }
      $res
    }"""
    //println(tree)
    c.Expr[Option[Int]](tree)
  }

  def findMacro(c: Context)(ts: c.Expr[Range])(fn: c.Expr[Int => Boolean]): c.Expr[Option[Int]] =
    findTree(c)(ts.tree, fn)

  def findMethod(c: Context)(fn: c.Expr[Int => Boolean]): c.Expr[Option[Int]] =
    findTree(c)(singleConsArg(c), fn)

  def forall(ts: Range)(fn: Int => Boolean): Boolean = macro forallMacro

  private[this] def forallTree[T](c: Context)(ts: c.Tree, fn: c.Expr[Int => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val end = newTerm(c, "end")
    val pos = newTerm(c, "pos")
    val step = newTerm(c, "step")
    val res = newTerm(c, "res")
    val rng = newTerm(c, "range")
    val tree = q"""{
      val $rng = $ts
      var $res = true
      var $pos = $rng.start
      val $end = $rng.end
      val $step = $rng.step
      while($res && ($pos < $end)) {
        val $arg = $pos
        $res = $newTree
        $pos += $step
      }
      $res
    }"""
    //println(tree)
    c.Expr[Boolean](tree)
  }
  def forallMacro(c: Context)(ts: c.Expr[Range])(fn: c.Expr[Int => Boolean]): c.Expr[Boolean] =
    forallTree(c)(ts.tree, fn)

  def forallMethod(c: Context)(fn: c.Expr[Int => Boolean]): c.Expr[Boolean] =
    forallTree(c)(singleConsArg(c), fn)

  def foreach(ts: Range)(fn: Int => Unit): Unit = macro foreachMacro

  private[this] def foreachTree(c: Context)(ts: c.Tree, fn: c.Expr[Int => Unit]): c.Expr[Unit] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val end = newTerm(c, "end")
    val pos = newTerm(c, "pos")
    val step = newTerm(c, "step")
    val rng = newTerm(c, "range")
    val tree = q"""{
      val $rng = $ts
      var $pos = $rng.start
      val $end = $rng.end
      val $step = $rng.step
      while($pos < $end) {
        val $arg = $pos
        $newTree
        $pos += $step
      }
      ()
    }"""
    //println(tree)
    c.Expr[Unit](tree)
  }
  def foreachMacro(c: Context)(ts: c.Expr[Range])(fn: c.Expr[Int => Unit]): c.Expr[Unit] =
    foreachTree(c)(ts.tree, fn)

  def foreachMethod(c: Context)(fn: c.Expr[Int => Unit]): c.Expr[Unit] =
    foreachTree(c)(singleConsArg(c), fn)

  def foldLeft[U](ts: Range, init: U)(fn: (U, Int) => U): U = macro foldLeftMacro[U]

  private[this] def foldLeftTree[U](c: Context)(ts: c.Tree, init: c.Expr[U], fn: c.Expr[(U, Int) => U]): c.Expr[U] = {
    import c.universe._
    //println(showRaw(fn))
    val (uarg, targ, newTree) = function2Apply(c)(fn)
    val end = newTerm(c, "end")
    val pos = newTerm(c, "pos")
    val step = newTerm(c, "step")
    val res = newTerm(c, "res")
    val rng = newTerm(c, "range")
    val tree = q"""{
      val $rng = $ts
      var $pos = $rng.start
      val $end = $rng.end
      var $uarg = $init
      val $step = $rng.step
      while($pos < $end) {
        val $targ = $pos
        $uarg = $newTree
        $pos += $step
      }
      $uarg
    }"""
    //println(tree)
    c.Expr[U](tree)
  }

  def foldLeftMacro[U](c: Context)(ts: c.Expr[Range], init: c.Expr[U])(fn: c.Expr[(U, Int) => U]): c.Expr[U] =
    foldLeftTree(c)(ts.tree, init, fn)

  def foldLeftMethod[U](c: Context)(init: c.Expr[U])(fn: c.Expr[(U, Int) => U]): c.Expr[U] =
    foldLeftTree(c)(singleConsArg(c), init, fn)

  def reduceOption(ts: Range)(fn: (Int, Int) => Int): Option[Int] = macro reduceOptionMacro

  private[this] def reduceOptionTree(c: Context)(ts: c.Tree, fn: c.Expr[(Int, Int) => Int]): c.Expr[Option[Int]] = {
    import c.universe._
    //println(showRaw(fn))
    val (uarg, targ, newTree) = function2Apply(c)(fn)
    val end = newTerm(c, "end")
    val pos = newTerm(c, "pos")
    val step = newTerm(c, "step")
    val res = newTerm(c, "res")
    val rng = newTerm(c, "range")
    val tree = q"""{
      val $rng = $ts
      var $pos = $rng.start
      val $end = $rng.end
      if ($pos >= $end) _root_.scala.None else {
        var $uarg = $pos
        val $step = $rng.step
        $pos += $step
        while($pos < $end) {
          val $targ = $pos
          $uarg = $newTree
          $pos += $step
        }
        _root_.scala.Some($uarg)
      }
    }"""
    //println(tree)
    c.Expr[Option[Int]](tree)
  }

  def reduceOptionMacro(c: Context)(ts: c.Expr[Range])(fn: c.Expr[(Int, Int) => Int]): c.Expr[Option[Int]] =
    reduceOptionTree(c)(ts.tree, fn)

  def reduceOptionMethod(c: Context)(fn: c.Expr[(Int, Int) => Int]): c.Expr[Option[Int]] =
    reduceOptionTree(c)(singleConsArg(c), fn)
}
