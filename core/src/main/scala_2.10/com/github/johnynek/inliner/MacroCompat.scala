package com.github.johnynek.inliner

object MacroCompat {
  type Context = scala.reflect.macros.Context

  def newTerm(c: Context, s: String) =
    c.universe.newTermName(c.fresh(s))

  // Somehow this resets the symbols... related to this:
  // http://stackoverflow.com/questions/20665778/splicing-a-passed-function-body-into-a-macro-rewritten-expression
  private[inliner] def recurse(c: Context)(on: c.Tree): c.Tree = {
    import c.universe._
    c.resetAllAttrs(on)
  }
  /*
   * This is purely to work around a bug with untypecheck creating incorrect trees
   */
  private[inliner] def convertUnapplyNonFatalToApply(c: Context)(on: c.Tree): c.Tree = {
    import c.universe._
    def eqn(t: Name, s: String): Boolean = t == newTermName(s)

    val transformer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case CaseDef(UnApply(Apply(Select(Select(Select(Select(Ident(scala), util), control), nonfatal), unapply), _), _), _, Apply(Select(New(failureType), c), _)) if (eqn(scala, "scala") && eqn(util, "util") && eqn(control, "control") && eqn(unapply, "unapply")) =>
          val applyTree = Apply(Select(New(failureType), c), List(Ident(newTermName("e"))))
          // Now use the apply:
          CaseDef(Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTermName("util")), newTermName("control")), newTermName("NonFatal")), List(Bind(newTermName("e"), Ident(nme.WILDCARD)))), EmptyTree, applyTree)
        case other => super.transform(other)
      }
    }
    transformer.transform(c.resetAllAttrs(on))
  }
  private[inliner] def tryBlock(c: Context)(block: c.Tree): c.Tree = {
    import c.universe._
    val failureType = TypeTree(typeOf[scala.util.Failure[Nothing]])
    val applyTree = Apply(Select(New(failureType), nme.CONSTRUCTOR), List(Ident(newTermName("e"))))
    // Now use the apply:
    val matchCase = CaseDef(Apply(Select(Select(Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTermName("util")), newTermName("control")), newTermName("NonFatal")), List(Bind(newTermName("e"), Ident(nme.WILDCARD)))), EmptyTree, applyTree)

    Try(block, List(matchCase), EmptyTree)
  }
  private[inliner] def singleConsArg(c: Context): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(_, List(arg)) => arg
      case Select(Apply(_, List(arg)), inline) if (inline == newTermName("inline")) => arg
      case other => c.abort(other.pos, s"Only works immediately following new Class allocation, e.g. (new Foo(x)), but found: ${showRaw(other)}")
    }
  }
}
