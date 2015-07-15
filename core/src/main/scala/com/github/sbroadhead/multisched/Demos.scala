package com.github.sbroadhead.multisched

import java.awt.Desktop
import java.io._
import java.nio.file._

import com.github.sbroadhead._
import multisched.functions._
import codegraph._
import scala.sys.process._

/**
 * Demos for the `multisched` frontend.
 */
object Demos {

  class ExpDotGraph extends Demo {
    override def run(args: Seq[String]): Unit = {
      val renderer = new FunctionDotRenderer(Exp.codeGraph)
      val dot = renderer.render

      val dotFile = File.createTempFile("multisched", ".dot")
      val bw = new BufferedWriter(new FileWriter(dotFile))
      bw.write(dot)
      bw.close()

      val viewer = if (args.isEmpty) { "/Applications/Graphviz.app/Contents/MacOS/Graphviz" } else args.head
      Seq(viewer, dotFile.getAbsolutePath).!
    }
  }

}
