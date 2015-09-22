package com.github.sbroadhead.multisched

import com.github.sbroadhead._
import codegraph._
import com.github.sbroadhead.codegraph.CodeGraph.{NodeKey, EdgeKey}
import com.github.sbroadhead.codegraph.dot.Dot.Builder
import dot._

import Instructions._
import Registers._

class FunctionDotRenderer(cg: FunctionGraph) extends CodeGraphRenderer[Register, Instruction](cg) {
  import CodeGraphOps._

  override def buildNodeLabelAttributes(key: NodeKey): Builder[(String, String)] = { attr =>
    val node = cg.node(key).head
    node match {
      case x if cg.inputs.contains(key) =>
        attr("shape", "triangle"); attr("style", "filled"); attr("label", cg.inputs.indexOf(key).toString)
      case x if cg.outputs.contains(key) =>
        attr("shape", "invtriangle"); attr("style", "filled"); attr("label", cg.outputs.indexOf(key).toString)
      case VEC() => attr("shape", "point"); attr("color", "#00a000")
    }
  }

  override def buildEdgeLabelAttributes(key: EdgeKey): Builder[(String, String)] = { attr =>
    val edge = cg.edge(key).head
    edge.label match {
      case const(Vec4(a,b,c,d)) =>
        attr("shape", "note")
        attr("fontname", "courier")
        attr("label", s"0x${a.toHexString}\\n0x${b.toHexString}\\n0x${c.toHexString}\\n0x${d.toHexString}")
        attr("style", "filled")
        attr("fillcolor", "#ffcccc")
      case x =>
        attr("shape", "rect")
        attr("fontname", "helvetica")
        attr("label", s"${edge.label.toString}")
        attr("style", "filled")
        attr("fillcolor", "#ccccff")
    }
  }
}
