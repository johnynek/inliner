package com.github.johnynek.inliner

object MacroCompat {
  type Context = scala.reflect.macros.blackbox.Context

  def newTerm(c: Context, s: String) =
    c.universe.TermName(c.freshName(s))

  // Somehow this resets the symbols... related to this:
  // http://stackoverflow.com/questions/20665778/splicing-a-passed-function-body-into-a-macro-rewritten-expression
  def recurse(c: Context)(on: c.Tree): c.Tree = {
    import c.universe._
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(name) => Ident(name)
        case other => super.transform(other)
      }
    }
    transformer.transform(c.untypecheck(on))
  }

  /*
   * This is purely to work around a bug with untypecheck creating incorrect trees
   */
  private[inliner] def convertUnapplyNonFatalToApply(c: Context)(on: c.Tree): c.Tree = {
    import c.universe._
    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case CaseDef(UnApply(Apply(Select(Select(Select(Select(Ident(TermName("scala")), TermName("util")), TermName("control")), TermName("NonFatal")), TermName("unapply")), _), _), _, Apply(Select(New(failureType), c), _))  =>
          val applyTree = Apply(Select(New(failureType), c), List(Ident(TermName("e"))))
          // Now use the apply:
          CaseDef(Apply(Select(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("util")), TermName("control")), TermName("NonFatal")), List(Bind(TermName("e"), Ident(termNames.WILDCARD)))), EmptyTree, applyTree)
        case other => super.transform(other)
      }
    }
    transformer.transform(c.untypecheck(on))
  }
  private[inliner] def tryBlock(c: Context)(block: c.Tree): c.Tree = {
    import c.universe._
    val failureType = TypeTree(typeOf[scala.util.Failure[Nothing]])
    val applyTree = Apply(Select(New(failureType), termNames.CONSTRUCTOR), List(Ident(TermName("e"))))
    // Now use the apply:
    val matchCase = CaseDef(Apply(Select(Select(Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TermName("util")), TermName("control")), TermName("NonFatal")), List(Bind(TermName("e"), Ident(termNames.WILDCARD)))), EmptyTree, applyTree)

    Try(block, List(matchCase), EmptyTree)
  }
  private[inliner] def singleConsArg(c: Context): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(_, List(arg)) => arg
      case Select(Apply(_, List(arg)), TermName("inline")) => arg
      case other => c.abort(other.pos, s"Only works immediately following new Class allocation, e.g. (new Foo(x)), but found: ${showRaw(other)}")
    }
  }
}
