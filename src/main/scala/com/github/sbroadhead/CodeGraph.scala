package com.github.sbroadhead

/**
 * A hypergraph with input and output nodes for representing control and data flow.
 * @tparam NKey The key type for indirectly storing nodes.
 * @tparam EKey The key type for indirectly storing edges.
 * @tparam N The node label type.
 * @tparam E The edge label type (i.e., instruction type)
 */
trait CodeGraph[NKey, EKey, N, E] {
  /**
   * A full specification of a hyper-edge.
   * @param op The edge label.
   * @param args The argument tentacles; a sequence of node keys.
   * @param results The result tentacles; a sequence of node keys.
   */
  case class Edge(op: E, args: Seq[NKey], results: Seq[NKey])

  /**
   * Returns the node keys that represent inputs to this codegraph.
   * @return The node keys.
   */
  def inputs: Seq[NKey]

  /**
   * Returns the node keys that represent outputs of this codegraph.
   * @return The node keys.
   */
  def outputs: Seq[NKey]

  /**
   * Returns a mapping from node keys to their labels.
   * @return The map.
   */
  def nodes: Map[NKey, N]

  /**
   * Returns a mapping from edge keys to the underlying edge structure.
   * @return The map.
   */
  def edges: Map[EKey, Edge]
}


/**
 * Companion object to [[CodeGraph]].
 */
object CodeGraph {
  /**
   * Implicitly wrap a [[CodeGraph]] in [[CodeGraphOps]] to support extended codegraph operations.
   * @param cg The underlying codegraph.
   * @tparam NKey Node key type.
   * @tparam EKey Edge key type.
   * @tparam N Node label.
   * @tparam E Edge label.
   * @return `cg` wrapped in an instance of [[CodeGraphOps]].
   */
  implicit def toCodeGraphOps[NKey, EKey, N, E](cg: CodeGraph[NKey, EKey, N, E]): CodeGraphOps[NKey, EKey, N, E] =
    new CodeGraphOps(cg)
}