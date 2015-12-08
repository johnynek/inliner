package com.github.johnynek.inliner

import scala.language.experimental.macros
import MacroCompat.{Context, newTerm, recurse, singleConsArg}

/**
 * This class is never allocated because all the macros require
 * them to be called as (new InlineOption(o).method) or it won't compile,
 * if it compiles, that bit is replaced with code using if/else
 */
class InlineCollection[T](o: TraversableOnce[T]) {
  import InlineCollection._

  def find(fn: T => Boolean): Option[T] = macro findMethod[T]
  def forall(fn: T => Boolean): Boolean = macro forallMethod[T]
  def foldLeft[U](init: U)(fn: (U, T) => U): U = macro foldLeftMethod[T, U]
  def foreach(fn: T => Unit): Unit = macro foreachMethod[T]
  def reduceOption(fn: (T, T) => T): Option[T] = macro reduceOptionMethod[T]
}
object InlineCollection {
  implicit class ToInlineList[T](val o: List[T]) extends AnyVal {
    @inline def inline: InlineCollection[T] = new InlineCollection(o)
  }
  implicit class ToInlineVector[T](val o: Vector[T]) extends AnyVal {
    @inline def inline: InlineCollection[T] = new InlineCollection(o)
  }
  implicit class ToInlineSeq[T](val o: Seq[T]) extends AnyVal {
    @inline def inline: InlineCollection[T] = new InlineCollection(o)
  }
  implicit class ToInlineIterable[T](val o: Iterable[T]) extends AnyVal {
    @inline def inline: InlineCollection[T] = new InlineCollection(o)
  }

  def find[T](ts: TraversableOnce[T])(fn: T => Boolean): Option[T] = macro findMacro[T]

  private[this] def findTree[T](c: Context)(ts: c.Tree, fn: c.Expr[T => Boolean])(implicit T: c.WeakTypeTag[T]): c.Expr[Option[T]] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val it = newTerm(c, "it")
    val res = newTerm(c, "res")
    val running = newTerm(c, "running")
    val tree = q"""{
      var $res: _root_.scala.Option[$T] = _root_.scala.None
      var $running = true
      val $it = $ts.toIterator
      while($running && $it.hasNext) {
        val $arg = $it.next
        if($newTree) { $res = _root_.scala.Some($arg); $running = false }
      }
      $res
    }"""
    //println(tree)
    c.Expr[Option[T]](tree)
  }
  def findMacro[T](c: Context)(ts: c.Expr[TraversableOnce[T]])(fn: c.Expr[T => Boolean])(implicit T: c.WeakTypeTag[T]): c.Expr[Option[T]] =
    findTree(c)(ts.tree, fn)

  def findMethod[T](c: Context)(fn: c.Expr[T => Boolean])(implicit T: c.WeakTypeTag[T]): c.Expr[Option[T]] =
    findTree(c)(singleConsArg(c), fn)

  def forall[T](ts: TraversableOnce[T])(fn: T => Boolean): Boolean = macro forallMacro[T]

  private[this] def forallTree[T](c: Context)(ts: c.Tree, fn: c.Expr[T => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val it = newTerm(c, "it")
    val res = newTerm(c, "res")
    val tree = q"""{
      var $res = true
      val $it = $ts.toIterator
      while($res && $it.hasNext) {
        val $arg = $it.next
        $res = $newTree
      }
      $res
    }"""
    //println(tree)
    c.Expr[Boolean](tree)
  }
  def forallMacro[T](c: Context)(ts: c.Expr[TraversableOnce[T]])(fn: c.Expr[T => Boolean]): c.Expr[Boolean] =
    forallTree(c)(ts.tree, fn)

  def forallMethod[T](c: Context)(fn: c.Expr[T => Boolean]): c.Expr[Boolean] =
    forallTree(c)(singleConsArg(c), fn)

  def foreach[T](ts: TraversableOnce[T])(fn: T => Unit): Unit = macro foreachMacro[T]

  private[this] def foreachTree[T](c: Context)(ts: c.Tree, fn: c.Expr[T => Unit]): c.Expr[Unit] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply[T, Unit](c)(fn)
    //println(showRaw(newTree))
    val it = newTerm(c, "it")
    val tree = q"""{ val $it = $ts.toIterator; while($it.hasNext) { val $arg = $it.next; $newTree } }"""
    //println(tree)
    c.Expr[Unit](tree)
  }
  def foreachMacro[T](c: Context)(ts: c.Expr[TraversableOnce[T]])(fn: c.Expr[T => Unit]): c.Expr[Unit] =
    foreachTree(c)(ts.tree, fn)

  def foreachMethod[T](c: Context)(fn: c.Expr[T => Unit]): c.Expr[Unit] =
    foreachTree(c)(singleConsArg(c), fn)

  def foldLeft[T, U](ts: TraversableOnce[T], init: U)(fn: (U, T) => U): U = macro foldLeftMacro[T, U]

  private[this] def foldLeftTree[T, U](c: Context)(ts: c.Tree, init: c.Expr[U], fn: c.Expr[(U, T) => U]): c.Expr[U] = {
    import c.universe._
    //println(showRaw(fn))
    val (uarg, targ, newTree) = function2Apply[U, T, U](c)(fn)
    val it = newTerm(c, "it")
    val tree = q"""{ val $it = $ts.toIterator; var $uarg = $init; while($it.hasNext) { val $targ = $it.next; $uarg = $newTree }; $uarg }"""
    //println(tree)
    c.Expr[U](tree)
  }

  def foldLeftMacro[T, U](c: Context)(ts: c.Expr[TraversableOnce[T]], init: c.Expr[U])(fn: c.Expr[(U, T) => U]): c.Expr[U] =
    foldLeftTree(c)(ts.tree, init, fn)

  def foldLeftMethod[T, U](c: Context)(init: c.Expr[U])(fn: c.Expr[(U, T) => U]): c.Expr[U] =
    foldLeftTree(c)(singleConsArg(c), init, fn)

  def reduceOption[T](ts: TraversableOnce[T])(fn: (T, T) => T): Option[T] = macro reduceOptionMacro[T]

  private[this] def reduceOptionTree[T](c: Context)(ts: c.Tree, fn: c.Expr[(T, T) => T]): c.Expr[Option[T]] = {
    import c.universe._
    //println(showRaw(fn))
    val (uarg, targ, newTree) = function2Apply(c)(fn)
    val it = newTerm(c, "it")
    val tree = q"""{
      val $it = $ts.toIterator;
      if($it.isEmpty) _root_.scala.None else _root_.scala.Some {
        var $uarg = $it.next;
        while($it.hasNext) {
          val $targ = $it.next;
          $uarg = $newTree
        }
        $uarg
      }
    }"""
    //println(tree)
    c.Expr[Option[T]](tree)
  }

  def reduceOptionMacro[T](c: Context)(ts: c.Expr[TraversableOnce[T]])(fn: c.Expr[(T, T) => T]): c.Expr[Option[T]] =
    reduceOptionTree(c)(ts.tree, fn)

  def reduceOptionMethod[T](c: Context)(fn: c.Expr[(T, T) => T]): c.Expr[Option[T]] =
    reduceOptionTree(c)(singleConsArg(c), fn)
}
