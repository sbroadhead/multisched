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
      case Nil => q"$prefix.withArgs(<>)"
      case x :: Nil => q"$prefix.withArgs(Tuple1($x))"
      case xs => q"$prefix.withArgs((..$xs))"
    }
  }

  def outputImpl(c: blackbox.Context)(args: c.Tree*): c.Tree = {
    import c.universe._
    val prefix = c.prefix
    args match {
      case Nil => q"$prefix.OutputConsumer().apply(<>)"
      case x :: Nil => q"$prefix.OutputConsumer().apply(Tuple1($x))"
      case xs => q"$prefix.OutputConsumer().apply((..$xs))"
    }
  }

  def spliceImpl(c: whitebox.Context)(cg: c.Tree, args: c.Tree*): c.Tree = {
    import c.universe._
    val prefix = c.prefix
    args match {
      case Nil => q"$prefix.Splicer($cg).apply(<>)"
      case x :: Nil => q"$prefix.Splicer($cg).apply(Tuple1($x))"
      case xs => q"$prefix.Splicer($cg).apply((..$xs))"
    }
  }
}
