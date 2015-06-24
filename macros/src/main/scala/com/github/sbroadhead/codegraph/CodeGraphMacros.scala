package com.github.sbroadhead.codegraph

import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox

/**
 * Macros related to `CodeGraph` creation.
 */
object CodeGraphMacros {
  def edgeApplyImpl(c: blackbox.Context)(args: c.Tree*): c.Tree = {
    import c.universe._
    val prefix = c.prefix
    args match {
      case Nil => q"$prefix.withArgs(())"
      case x :: Nil => q"$prefix.withArgs(Tuple1($x))"
      case xs => q"$prefix.withArgs((..$xs))"
    }
  }

  def inputsImpl(c: whitebox.Context)(args: c.Tree*): c.Tree = {
    import c.universe._
    val applyArgs = args.map(arg => q"$arg.apply()")
    q"inputInstantiator[(..$applyArgs)].apply()"
  }
}
