package com.github.sbroadhead.multisched

import java.awt.Desktop
import java.nio.file._

import com.github.sbroadhead._
import multisched.functions._
import codegraph._

/**
 * Demos for the `multisched` frontend.
 */
object Demos {

  class ExpDotGraph extends Demo {
    override def run(args: Seq[String]): Unit = {
      import Registers._
      import Instructions._
      val renderer = new FunctionDotRenderer(Exp.codeGraph)
      val dot = renderer.render
      println(dot)
    }
  }

}
