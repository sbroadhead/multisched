package com.github.sbroadhead

/**
 * A hypergraph with input and output nodes for representing control and data flow, with
 * mutators for safely modifying the contents of the graph in-place.
 * @tparam NKey The key type for indirectly storing nodes.
 * @tparam EKey The key type for indirectly storing edges.
 * @tparam N The node label type.
 * @tparam E The edge label type (i.e., instruction type)
 */
trait MutableCodeGraph[NKey, EKey, N, E] extends CodeGraph[NKey, EKey, N, E] {
  /**
   * Add a node to this codegraph.
   * @param n The label of the new node.
   * @return The key of the new node.
   */
  def addNode(n: N): NKey

  /**
   * Add an edge to this codegraph.
   * @param e The label of the new node.
   * @param args The argument node keys.
   * @param results The result node keys.
   * @return The key of the new edge.
   */
  def addEdge(e: E, args: Seq[NKey], results: Seq[NKey]): EKey

  /**
   * Set the inputs of this codegraph.
   * @param in The inputs.
   */
  def inputs_=(in: Seq[NKey]): Unit

  /**
   * Set the outputs of this codegraph.
   * @param out The outputs.
   */
  def outputs_=(out: Seq[NKey]): Unit
}
