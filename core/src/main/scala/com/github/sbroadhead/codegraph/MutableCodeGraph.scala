package com.github.sbroadhead.codegraph

import scala.collection.mutable.{Map => MutableMap, Seq => MutableSeq}

/**
 * A mutable [[CodeGraph]] instance.
 */
class MutableCodeGraph[N, E] extends CodeGraph[N, E] with CodeGraphInterface {
  import CodeGraph._

  /** Code graph inputs. */
  var inputs: MutableSeq[NodeKey] = MutableSeq()

  /**  Code graph outputs. */
  var outputs: MutableSeq[NodeKey] = MutableSeq()

  /** Nodes in this code graph. */
  var nodes: MutableMap[NodeKey, N] = MutableMap()

  /** Edges in this code graph. */
  var edges: MutableMap[EdgeKey, Edge[E]] = MutableMap()
}

/**
 * Companion object for [[MutableCodeGraph]].
 */
object MutableCodeGraph {
  def apply[N, E](cg: CodeGraph[N, E]): MutableCodeGraph[N, E] = {
    val builder = new MutableCodeGraph[N, E]
    builder.inputs = MutableSeq(cg.inputs : _*)
    builder.outputs = MutableSeq(cg.outputs : _*)
    builder.nodes = MutableMap(cg.nodes.toSeq : _*)
    builder.edges = MutableMap(cg.edges.toSeq : _*)
    builder
  }

  def apply[N, E, Input0 <: Product, Output0 <: Product]
    (cg: CodeGraph[N, E] with CodeGraphInterface.Aux[Input0, Output0]): MutableCodeGraph.Aux[N, E, Input0, Output0] = {
      val builder = new MutableCodeGraph[N, E] { type Input = Input0; type Output = Output0 }
      builder.inputs = MutableSeq(cg.inputs : _*)
      builder.outputs = MutableSeq(cg.outputs : _*)
      builder.nodes = MutableMap(cg.nodes.toSeq : _*)
      builder.edges = MutableMap(cg.edges.toSeq : _*)
      builder
    }

  type Aux[N, E, Input0, Output0] = MutableCodeGraph[N, E] {
    type Input = Input0
    type Output = Output0
  }

  implicit class CodeGraphExts[N, E](val cg: CodeGraph[N, E]) extends AnyVal {
    def mutable: MutableCodeGraph[N, E] = MutableCodeGraph(cg)
  }

}