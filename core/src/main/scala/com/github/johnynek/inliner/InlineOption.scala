package com.github.johnynek.inliner

import scala.language.experimental.macros
import MacroCompat.{Context, newTerm, recurse, singleConsArg}

/**
 * This class is never allocated because all the macros require
 * them to be called as (new InlineOption(o).method) or it won't compile,
 * if it compiles, that bit is replaced with code using if/else
 */
class InlineOption[T](o: Option[T]) {
  import InlineOption._

  def filter(fn: T => Boolean): Option[T] = macro optionFilterMethod[T]
  def flatMap[U](fn: T => Option[U]): Option[U] = macro optionFlatMapMethod[T, U]
  def flatten[U](implicit ev: T <:< Option[U]): Option[U] = macro optionFlattenMethod[T, U]
  // note that u is evaluated lazily inside the else branch on the isDefined check
  def fold[U](u: U)(fn: T => U): U = macro optionFoldMethod[T, U]
  def foreach(fn: T => Unit): Unit = macro optionForeachMethod[T]
  // Note, this is still lazy in t since it winds up evaluating t in an else-branch
  def getOrElse(t: T): T = macro getOrElseMethod[T]
  def map[U](fn: T => U): Option[U] = macro optionMapMethod[T, U]
  def orElse[U >: T](that: Option[U]): Option[U] = macro optionOrElseMethod[U]
}
/**
 * import option.ToInline to get the .inline syntax for Options
 */
object InlineOption {
  implicit class ToInline[T](val o: Option[T]) extends AnyVal {
    @inline def inline: InlineOption[T] = new InlineOption(o)
  }

  def filter[T](o: Option[T])(fn: T => Boolean): Option[T] = macro optionFilterMacro[T]

  def optionFilterMacro[T](c: Context)(o: c.Expr[Option[T]])(fn: c.Expr[T => Boolean]): c.Expr[Option[T]] =
    optionFilterTree(c)(o.tree, fn)

  def optionFilterMethod[T](c: Context)(fn: c.Expr[T => Boolean]): c.Expr[Option[T]] =
    optionFilterTree(c)(singleConsArg(c), fn)

  private def optionFilterTree[T](c: Context)(o: c.Tree, fn: c.Expr[T => Boolean]): c.Expr[Option[T]] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $o;
      if ($opt.isDefined) { val $arg = $opt.get; if ($newTree) $opt else _root_.scala.None }
      else _root_.scala.None }"""
    //println(tree)
    c.Expr[Option[T]](tree)
  }

  def flatMap[T, U](o: Option[T])(fn: T => Option[U]): Option[U] = macro optionFlatMapMacro[T, U]

  def optionFlatMapMacro[T, U](c: Context)(o: c.Expr[Option[T]])(fn: c.Expr[T => Option[U]]): c.Expr[Option[U]] =
    optionFlatMapTree(c)(o.tree, fn)

  def optionFlatMapMethod[T, U](c: Context)(fn: c.Expr[T => Option[U]]): c.Expr[Option[U]] =
    optionFlatMapTree(c)(singleConsArg(c), fn)

  private def optionFlatMapTree[T, U](c: Context)(o: c.Tree, fn: c.Expr[T => Option[U]]): c.Expr[Option[U]] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $o; if ($opt.isDefined) { val $arg = $opt.get; $newTree } else _root_.scala.None }"""
    //println(tree)
    c.Expr[Option[U]](tree)
  }
  def flatten[T](o: Option[Option[T]]): Option[T] = macro optionFlattenMacro[T]

  def optionFlattenMacro[T](c: Context)(o: c.Expr[Option[Option[T]]]): c.Expr[Option[T]] =
    optionFlattenTree[T](c)(o.tree)

  def optionFlattenMethod[T, U](c: Context)(ev: c.Expr[T <:< Option[U]]): c.Expr[Option[U]] =
    optionFlattenTree[U](c)(singleConsArg(c))

  private def optionFlattenTree[U](c: Context)(o: c.Tree): c.Expr[Option[U]] = {
    import c.universe._
    //println(showRaw(fn))
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $o; if ($opt.isDefined) $opt.get else _root_.scala.None }"""
    //println(tree)
    c.Expr[Option[U]](tree)
  }

  def fold[T, U](o: Option[T], u: U)(fn: T => U): U = macro optionFoldMacro[T, U]

  def optionFoldMacro[T, U](c: Context)(o: c.Expr[Option[T]], u: c.Expr[U])(fn: c.Expr[T => U]): c.Expr[U] =
    optionFoldTree(c)(o.tree, u, fn)

  def optionFoldMethod[T, U](c: Context)(u: c.Expr[U])(fn: c.Expr[T => U]): c.Expr[U] =
    optionFoldTree(c)(singleConsArg(c), u, fn)

  private[this] def optionFoldTree[T, U](c: Context)(optT: c.Tree, u: c.Expr[U], fn: c.Expr[T => U]): c.Expr[U] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply[T, U](c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $optT; if ($opt.isDefined) { val $arg = $opt.get; $newTree } else { $u } }"""
    //println(tree)
    c.Expr[U](tree)
  }

  def foreach[T](o: Option[T])(fn: T => Unit): Unit = macro optionForeachMacro[T]

  def optionForeachMacro[T](c: Context)(o: c.Expr[Option[T]])(fn: c.Expr[T => Unit]): c.Expr[Unit] =
    optionForeachTree(c)(o.tree, fn)

  def optionForeachMethod[T](c: Context)(fn: c.Expr[T => Unit]): c.Expr[Unit] =
    optionForeachTree(c)(singleConsArg(c), fn)

  private[this] def optionForeachTree[T](c: Context)(o: c.Tree, fn: c.Expr[T => Unit]): c.Expr[Unit] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $o; if ($opt.isDefined) { val $arg = $opt.get; $newTree } else () }"""
    //println(tree)
    c.Expr[Unit](tree)
  }

  def map[T, U](o: Option[T])(fn: T => U): Option[U] = macro optionMapMacro[T, U]

  private[this] def optionMapTree[T, U](c: Context)(o: c.Tree, fn: c.Expr[T => U]): c.Expr[Option[U]] = {
    import c.universe._
    //println(showRaw(fn))
    val tree = if (isIdentityFn(c)(fn)) {
      q"""$o"""
    }
    else {
      val (arg, newTree) = function1Apply(c)(fn)
      //println(showRaw(newTree))
      val opt = newTerm(c, "opt")
      q"""{ val $opt = $o; if ($opt.isDefined) { val $arg = $opt.get; _root_.scala.Some($newTree) } else _root_.scala.None }"""
    }
    //println(tree)
    c.Expr[Option[U]](tree)
  }
  def optionMapMethod[T, U](c: Context)(fn: c.Expr[T => U]): c.Expr[Option[U]] =
    optionMapTree(c)(singleConsArg(c), fn)
  def optionMapMacro[T, U](c: Context)(o: c.Expr[Option[T]])(fn: c.Expr[T => U]): c.Expr[Option[U]] =
    optionMapTree(c)(o.tree, fn)

  def getOrElse[T](o: Option[T], t: T): T = macro getOrElseMacro[T]
  private[this] def getOrElseTree[T](c: Context)(o: c.Tree, t: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $o; if ($opt.isDefined) $opt.get else $t }"""
    c.Expr[T](tree)
  }
  def getOrElseMacro[T](c: Context)(o: c.Expr[Option[T]], t: c.Expr[T]): c.Expr[T] =
    getOrElseTree(c)(o.tree, t)
  def getOrElseMethod[T](c: Context)(t: c.Expr[T]): c.Expr[T] =
    getOrElseTree(c)(singleConsArg(c), t)

  def orElse[T](o: Option[T], t: Option[T]): Option[T] = macro optionOrElseMacro[T]
  private[this] def orElseTree[U](c: Context)(o: c.Tree, t: c.Expr[Option[U]]): c.Expr[Option[U]] = {
    import c.universe._
    val opt = newTerm(c, "opt")
    val tree = q"""{ val $opt = $o; if ($opt.isDefined) $opt else $t }"""
    //println(tree)
    c.Expr[Option[U]](tree)
  }
  def optionOrElseMacro[T](c: Context)(o: c.Expr[Option[T]], t: c.Expr[Option[T]]): c.Expr[Option[T]] =
    orElseTree(c)(o.tree, t)
  def optionOrElseMethod[U](c: Context)(that: c.Expr[Option[U]]): c.Expr[Option[U]] =
    orElseTree(c)(singleConsArg(c), that)
}
