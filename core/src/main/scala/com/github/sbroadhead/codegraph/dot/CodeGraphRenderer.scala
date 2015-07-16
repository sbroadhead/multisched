package com.github.sbroadhead.codegraph.dot

import com.github.sbroadhead._
import codegraph._
import com.github.sbroadhead.codegraph.CodeGraph.{EdgeKey, NodeKey}

/**
 * Rendering support for CodeGraphs
 */
class CodeGraphRenderer[N, E](cg: CodeGraph[N, E]) {
  import Dot._
  import CodeGraphOps._

  /**
   * Return a string containing a DOT attribute list for a node
   * @return The attribute list string
   */
  def buildNodeLabelAttributes(key: NodeKey): Builder[(String, String)] = { attr =>
    if (cg.inputs.contains(key)) {
      attr("shape", "triangle")
      attr("label", cg.inputs.indexOf(key).toString)
    } else if (cg.outputs.contains(key)) {
      attr("shape", "invtriangle")
      attr("label", cg.outputs.indexOf(key).toString)
    } else {
      attr("shape", "point")
    }
  }

  /**
   * Return a string containing a DOT attribute list for an edge
   * @return The edge list string
   */
  def buildEdgeLabelAttributes(key: EdgeKey): Builder[(String, String)] = { attr =>
    val edge = cg.edge(key)
    attr("shape", "rectangle")
    attr("label", edge.toString())
  }

  /**
   * Render a CodeGraph to a DOT string.
   * @return The DOT string
   */
  def render: String =
    digraph(cg.hashCode.toString) { stmt =>
      for ((k, n) <- cg.nodes)
        stmt(node(s"n${k.toHexString}")(buildNodeLabelAttributes(k)))
      for ((k, e) <- cg.edges) {
        stmt(node(s"e${k.toHexString}")(buildEdgeLabelAttributes(k)))
        for (src <- e.args.zipWithIndex) stmt(edge(Seq(s"n${src._1.toHexString}", s"e${k.toHexString}")) { attr =>
          attr("headlabel" -> src._2.toString)
          attr("fontname" -> "helvetica")
        })
        for (trg <- e.results.zipWithIndex) stmt(edge(Seq(s"e${k.toHexString}", s"n${trg._1.toHexString}")) { attr =>
        })
      }
    }
}
