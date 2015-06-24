package com.github.sbroadhead.codegraph

import scala.language.experimental.macros
import scala.collection.mutable.{Seq => MutableSeq}
import shapeless._

/**
 * Mutator functions for [[MutableCodeGraph]]
 */
class CodeGraphMutators[N, E](cg: MutableCodeGraph[N, E]) {
  /**
   * Returns a new [[NodeNameInstantiator]] to instantiate a tuple of [[NodeName]] instances
   * reflecting the types given by `Args`
   * @tparam Args A tuple of node label types to encode in the [[NodeName]] instances.
   * @return A new [[NodeNameInstantiator]].
   */
  def inputInstantiator[Args <: Product](implicit nni: NodeNameInstantiator[Args]) =
    new NodeNameInstantiator[Args] {
      type Out = nni.Out
      override def apply(): nni.Out = {
        val newNodes = nni.apply()
        val nodesTyped = newNodes.productIterator.map(_.asInstanceOf[NodeName[N]]).toSeq
        addNodes(nodesTyped)
        cg.inputs = MutableSeq(nodesTyped.map { case NodeName(_, key) => key }.toSeq : _*)
        newNodes
      }
    }

  /**
   * Convenient macro-based helper for `inputInstantiator`. Call with a sequence of types
   * and it will be automatically transformed into the uglier `inputInstantiator` call.
   * @param args A sequence of types to use for input nodes.
   */
  def input(args: Any*): Any = macro CodeGraphMacros.inputsImpl

  /**
   * Use a sequence of nodes as outputs to this code graph.
   * @param outputs The outputs.
   */
  def output(outputs: NodeName[_]*): Unit = {
    cg.outputs = MutableSeq(outputs.map { case NodeName(_, key) => key } : _*)
  }

  /**
   * Helper class for applying arguments to a strongly-typed edge label (that has
   * the [[EdgeLabel]] trait) in a strongly-typed fashion.
   */
  implicit class Op[Args <: Product, Results <: Product, ATup <: Product, RTup <: Product]
    (label: E with EdgeLabel[Args, Results])
    (implicit
      nntm: NodeNameTupleMapped.Aux[Args, ATup],
      inst: NodeNameInstantiator.Aux[Results, RTup])
  {
    /**
     * Apply the given arguments to this edge label and add the resulting edge
     * to the underlying [[CodeGraph]].
     * @param args the arguments to this edge.
     * @return the result nodes of this edge.
     */
    def withArgs(args: ATup): RTup = {
      val resultNodes = inst.apply()
      val resultsTyped = resultNodes.productIterator.map { x => x.asInstanceOf[NodeName[N]] }
      addNodes(resultsTyped.toSeq)
      val edge = {
        val argKeys = args.productIterator.map { case NodeName(_, key) => key }
        val resKeys = resultNodes.productIterator.map { case NodeName(_, key) => key }
        new CodeGraph.Edge[E](label, argKeys.toSeq, resKeys.toSeq)
      }
      addEdge(edge)
      resultNodes
    }

    /**
     * Helper macro to mimic function application to produce edges; wraps `withArgs`.
     */
    def $(args: Any*): RTup = macro CodeGraphMacros.edgeApplyImpl
  }

  /**
   * Add a node to this CodeGraph and return the key.
   * @param node The node to add.
   * @return The key of the newly added node.
   */
  def addNode(node: NodeName[N]): CodeGraph.NodeKey = {
    cg.nodes += node.key -> node.label
    node.key
  }

  /**
   * Add an edge to this CodeGraph and return the key.
   * @param edge The edge to add.
   * @return The key of the newly added edge.
   */
  def addEdge(edge: CodeGraph.Edge[E]): CodeGraph.EdgeKey = {
    val newKey = 1 + cg.edges.keys.reduceOption(_ max _).getOrElse(0: CodeGraph.EdgeKey)
    cg.edges(newKey) = edge
    newKey
  }

  private def addNodes(nodes: Seq[NodeName[N]]): Unit = {
    for (node <- nodes) {
      cg.nodes(node.key) = node.label
    }
  }
}