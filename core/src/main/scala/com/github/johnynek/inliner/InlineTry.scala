package com.github.johnynek.inliner

import scala.language.experimental.macros
import MacroCompat.{Context, convertUnapplyNonFatalToApply, tryBlock, newTerm, recurse, singleConsArg}

import scala.util.{Try => suTry}

/**
 * import inlineTry.ToInline to get the .inline syntax for suTrys
 */
object InlineTry {
  implicit class ToInlineTry[T](val o: suTry[T]) extends AnyVal {
    @inline def inline: InlinesuTry[T] = new InlinesuTry(o)
  }
  /**
   * This class is never allocated because all the macros require
   * them to be called as (new InlinesuTry(o).method) or it won't compile,
   * if it compiles, that bit is replaced with code using if/else
   */
  class InlinesuTry[T](o: suTry[T]) {
    def filter(fn: T => Boolean): suTry[T] = macro inlineTryFilterMethod[T]
    def flatMap[U](fn: T => suTry[U]): suTry[U] = macro inlineTryFlatMapMethod[T, U]
    def flatten[U](implicit ev: T <:< suTry[U]): suTry[U] = macro inlineTryFlattenMethod[T, U]
    def fold[U](f: Throwable => U, s: T => U): U = macro inlineTryFoldMethod[T, U]
    def foreach(fn: T => Unit): Unit = macro inlineTryForeachMethod[T]
    // Note, this is still lazy in t since it winds up evaluating t in an else-branch
    def getOrElse(t: T): T = macro getOrElseMethod[T]
    def map[U](fn: T => U): suTry[U] = macro inlineTryMapMethod[T, U]
    def orElse[U >: T](u: suTry[U]): suTry[U] = macro inlineTryOrElseMethod[U]
  }

  def inlineTry[T](t: T): suTry[T] = macro inlineTryMacro[T]

  // This is try x catch { case NonFatal(e) => Failure(u) }
  private[this] def applyMacro[T](c: Context)(tree: c.Tree)(implicit T: c.WeakTypeTag[T]): c.Tree = {
    import c.universe._
    tryBlock(c)(recurse(c)(tree))
  }

  def inlineTryMacro[T](c: Context)(t: c.Expr[T])(implicit T: c.WeakTypeTag[T]): c.Expr[suTry[T]] = {
    import c.universe._
    val resTree = applyMacro(c)(q"""new _root_.scala.util.Success[$T]($t)""")
    //println(resTree)
    c.Expr[suTry[T]](resTree)
  }

  def filter[T](o: suTry[T])(fn: T => Boolean): suTry[T] = macro inlineTryFilterMacro[T]

  def inlineTryFilterMacro[T](c: Context)(o: c.Expr[suTry[T]])(fn: c.Expr[T => Boolean])(implicit T: c.WeakTypeTag[T]): c.Expr[suTry[T]] =
    inlineTryFilterTree(c)(o.tree, fn)

  def inlineTryFilterMethod[T](c: Context)(fn: c.Expr[T => Boolean])(implicit T: c.WeakTypeTag[T]): c.Expr[suTry[T]] =
    inlineTryFilterTree(c)(singleConsArg(c), fn)

  private def inlineTryFilterTree[T](c: Context)(o: c.Tree, fn: c.Expr[T => Boolean])(implicit T: c.WeakTypeTag[T]): c.Expr[suTry[T]] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{ val $opt = $o;
      if ($opt.isSuccess) {
        val $arg = $opt.get
        if ($newTree) $opt
        else new _root_.scala.util.Failure[$T](new _root_.java.util.NoSuchElementException("Predicate does not hold for " + $arg))
      }
      else $opt
    }""")
   //println(tree)
    c.Expr[suTry[T]](tree)
  }

  def flatMap[T, U](o: suTry[T])(fn: T => suTry[U]): suTry[U] = macro inlineTryFlatMapMacro[T, U]

  def inlineTryFlatMapMacro[T, U](c: Context)(o: c.Expr[suTry[T]])(fn: c.Expr[T => suTry[U]])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] =
    inlineTryFlatMapTree(c)(o.tree, fn)

  def inlineTryFlatMapMethod[T, U](c: Context)(fn: c.Expr[T => suTry[U]])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] =
    inlineTryFlatMapTree(c)(singleConsArg(c), fn)

  private def inlineTryFlatMapTree[T, U](c: Context)(o: c.Tree, fn: c.Expr[T => suTry[U]])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{
      val $opt = $o
      if ($opt.isSuccess) {
        val $arg = $opt.get
        ${applyMacro[U](c)(newTree)}
      }
      else ($opt.asInstanceOf[_root_.scala.util.Try[$U]])
    }""")
   //println(tree)
   //println(showRaw(tree))
    c.Expr[suTry[U]](tree)
  }
  def flatten[T](o: suTry[suTry[T]]): suTry[T] = macro inlineTryFlattenMacro[T]

  def inlineTryFlattenMacro[T](c: Context)(o: c.Expr[suTry[suTry[T]]])(implicit T: c.WeakTypeTag[T]): c.Expr[suTry[T]] =
    inlineTryFlattenTree[T](c)(o.tree)

  def inlineTryFlattenMethod[T, U](c: Context)(ev: c.Expr[T <:< suTry[U]])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] =
    inlineTryFlattenTree[U](c)(singleConsArg(c))

  private def inlineTryFlattenTree[U](c: Context)(o: c.Tree)(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] = {
    import c.universe._
    //println(showRaw(fn))
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{
      val $opt = $o
      if ($opt.isSuccess) $opt.get
      else ($opt.asInstanceOf[_root_.scala.util.Try[$U]])
    }""")
   //println(tree)
    c.Expr[suTry[U]](tree)
  }

  def fold[T, U](o: suTry[T])(f: Throwable => U, s: T => U): U = macro inlineTryFoldMacro[T, U]

  def inlineTryFoldMacro[T, U](c: Context)(o: c.Expr[suTry[T]])(f: c.Expr[Throwable => U], s: c.Expr[T => U]): c.Expr[U] =
    inlineTryFoldTree(c)(o.tree, f, s)

  def inlineTryFoldMethod[T, U](c: Context)(f: c.Expr[Throwable => U], s: c.Expr[T => U]): c.Expr[U] =
    inlineTryFoldTree(c)(singleConsArg(c), f, s)

  private[this] def inlineTryFoldTree[T, U](c: Context)(optT: c.Tree, f: c.Expr[Throwable => U], s: c.Expr[T => U]): c.Expr[U] = {
    import c.universe._
    //println(showRaw(fn))
    val (farg, fTree) = function1Apply(c)(f)
    val (sarg, sTree) = function1Apply(c)(s)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{
      val $opt = $optT
      if ($opt.isSuccess) {
        val $sarg = $opt.get
        $sTree
      }
      else {
        val $farg = $opt.asInstanceOf[_root_.scala.util.Failure[_]].exception
        $fTree
      }
    }""")
   //println(tree)
    c.Expr[U](tree)
  }

  def foreach[T](o: suTry[T])(fn: T => Unit): Unit = macro inlineTryForeachMacro[T]

  def inlineTryForeachMacro[T](c: Context)(o: c.Expr[suTry[T]])(fn: c.Expr[T => Unit]): c.Expr[Unit] =
    inlineTryForeachTree(c)(o.tree, fn)

  def inlineTryForeachMethod[T](c: Context)(fn: c.Expr[T => Unit]): c.Expr[Unit] =
    inlineTryForeachTree(c)(singleConsArg(c), fn)

  private[this] def inlineTryForeachTree[T](c: Context)(o: c.Tree, fn: c.Expr[T => Unit]): c.Expr[Unit] = {
    import c.universe._
    //println(showRaw(fn))
    val (arg, newTree) = function1Apply(c)(fn)
    //println(showRaw(newTree))
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{ val $opt = $o; if ($opt.isSuccess) { val $arg = $opt.get; $newTree } else () }""")
   //println(tree)
    c.Expr[Unit](tree)
  }

  def map[T, U](o: suTry[T])(fn: T => U): suTry[U] = macro inlineTryMapMacro[T, U]

  private[this] def inlineTryMapTree[T, U](c: Context)(o: c.Tree, fn: c.Expr[T => U])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] = {
    import c.universe._
   //println(showRaw(fn))
    val tree = if (isIdentityFn(c)(fn)) {
      convertUnapplyNonFatalToApply(c)(q"""$o""")
    }
    else {
      val (arg, newTree) = function1Apply(c)(fn)
      val successTree = q"""new _root_.scala.util.Success($newTree)"""
      //println(showRaw(newTree))
      val opt = newTerm(c, "opt")
      convertUnapplyNonFatalToApply(c)(q"""{
        val $opt = $o
        if ($opt.isSuccess) {
          val $arg = $opt.get
          ${applyMacro[U](c)(successTree)}
        }
        else $opt.asInstanceOf[_root_.scala.util.Try[$U]]
      }
      """)
    }
  //println(tree)
    c.Expr[suTry[U]](tree)
  }
  def inlineTryMapMethod[T, U](c: Context)(fn: c.Expr[T => U])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] =
    inlineTryMapTree(c)(singleConsArg(c), fn)
  def inlineTryMapMacro[T, U](c: Context)(o: c.Expr[suTry[T]])(fn: c.Expr[T => U])(implicit U: c.WeakTypeTag[U]): c.Expr[suTry[U]] =
    inlineTryMapTree(c)(o.tree, fn)

  def getOrElse[T](o: suTry[T], t: T): T = macro getOrElseMacro[T]
  private[this] def getOrElseTree[T](c: Context)(o: c.Tree, t: c.Expr[T]): c.Expr[T] = {
    import c.universe._
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{ val $opt = $o; if ($opt.isSuccess) $opt.get else $t }""")
    c.Expr[T](tree)
  }
  def getOrElseMacro[T](c: Context)(o: c.Expr[suTry[T]], t: c.Expr[T]): c.Expr[T] =
    getOrElseTree(c)(o.tree, t)
  def getOrElseMethod[T](c: Context)(t: c.Expr[T]): c.Expr[T] =
    getOrElseTree(c)(singleConsArg(c), t)

  def orElse[T](o: suTry[T], t: suTry[T]): suTry[T] = macro inlineTryOrElseMacro[T]
  private[this] def orElseTree[U](c: Context)(o: c.Tree, t: c.Expr[suTry[U]]): c.Expr[suTry[U]] = {
    import c.universe._
    val opt = newTerm(c, "opt")
    val tree = convertUnapplyNonFatalToApply(c)(q"""{ val $opt = $o; if ($opt.isSuccess) $opt else $t }""")
   //println(tree)
    c.Expr[suTry[U]](tree)
  }
  def inlineTryOrElseMacro[T](c: Context)(o: c.Expr[suTry[T]], t: c.Expr[suTry[T]]): c.Expr[suTry[T]] =
    orElseTree(c)(o.tree, t)
  def inlineTryOrElseMethod[U](c: Context)(u: c.Expr[suTry[U]]): c.Expr[suTry[U]] =
    orElseTree(c)(singleConsArg(c), u)
}
