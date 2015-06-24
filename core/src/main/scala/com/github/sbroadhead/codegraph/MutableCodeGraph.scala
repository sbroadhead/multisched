package com.github.sbroadhead.codegraph

import scala.collection.mutable.{Map => MutableMap, Seq => MutableSeq}

/**
 * A mutable [[CodeGraph]] instance.
 */
class MutableCodeGraph[N, E] extends CodeGraph[N, E] {
  import CodeGraph._

  /** Code graph inputs. */
  var inputs: MutableSeq[NodeKey] = MutableSeq()

  /**  Code graph outputs. */
  var outputs: MutableSeq[NodeKey] = MutableSeq()

  /** Nodes in this code graph. */
  var nodes: MutableMap[NodeKey, N] = MutableMap()

  /** Edges in this code graph. */
  var edges: MutableMap[EdgeKey, Edge[E]] = MutableMap()

  /** A mutator instance which contains implicit mutators for this type of codegraph. */
  object mutators extends CodeGraphMutators[N, E](this)
}

/**
 * Companion object for [[MutableCodeGraph]].
 */
object MutableCodeGraph {
  /**
   * Create a [[MutableCodeGraph]] for the given [[CodeGraph]].
   * @param cg The [[CodeGraph]] to create a builder for.
   * @tparam N The node label type.
   * @tparam E The edge label type.
   * @return The newly created [[MutableCodeGraph]].
   */
  def apply[N, E](cg: CodeGraph[N, E]): MutableCodeGraph[N, E] = {
    val builder = new MutableCodeGraph[N, E]
    builder.inputs = MutableSeq(cg.inputs : _*)
    builder.outputs = MutableSeq(cg.outputs : _*)
    builder.nodes = MutableMap(cg.nodes.toSeq : _*)
    builder.edges = MutableMap(cg.edges.toSeq : _*)
    builder
  }

  implicit class CodeGraphExts[N, E](val cg: CodeGraph[N, E]) extends AnyVal {
    def mutable: MutableCodeGraph[N, E] = MutableCodeGraph(cg)
  }

}