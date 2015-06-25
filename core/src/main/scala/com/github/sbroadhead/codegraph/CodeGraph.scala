package com.github.sbroadhead.codegraph

import scala.collection._

/**
 * Typed hyper-graph representing a computation with nodes labelled by data types
 * and edges labelled by operations.
 * @tparam N Node label type.
 * @tparam E Edge label type.
 */
trait CodeGraph[N, E] {
  import CodeGraph._

  /** The node label type */
  type NodeType = N

  /** The edge label type */
  type EdgeType = E

  /**
   * Return the inputs to this code graph.
   * @return the inputs to this code graph.
   */
  def inputs: Seq[NodeKey]

  /**
   * Return the outputs from this code graph.
   * @return the outputs from this code graph.
   */
  def outputs: Seq[NodeKey]

  /**
   * Return the nodes in this code graph, as a map from keys to labels.
   * @return the nodes in this code graph.
   */
  def nodes: Map[NodeKey, N]

  /**
   * Return the edges in this code graph, as a map from edge number to [[CodeGraph.Edge]].
   * @return the nodes in this code graph.
   */
  def edges: Map[EdgeKey, Edge[E]]

  override def toString =
    s"CodeGraph(inputs=${inputs.toList}, outputs=${outputs.toList}, nodes=${nodes.toList}, edges=${edges.toList})"
}

/**
 * Companion object for [[CodeGraph]].
 */
object CodeGraph {
  type NodeKey = Long
  type EdgeKey = Long

  /**
   * Construct an immutable [[CodeGraph]] given the structural components.
   * @return the newly constructed [[CodeGraph]].
   */
  def apply[N, E]
    (nodes0: Map[NodeKey, N], edges0: Map[EdgeKey, Edge[E]], inputs0: Seq[NodeKey], outputs0: Seq[NodeKey]): CodeGraph[N, E] =
      new CodeGraph[N, E] {
        val nodes = nodes0
        val edges = edges0
        val inputs = inputs0
        val outputs = outputs0
      }

  /**
   * An edge label along with source and target tentacles.
   * @param label The edge label.
   * @param args The source tentacles.
   * @param results The target tentacles.
   */
  class Edge[+E](val label: E, val args: Seq[NodeKey], val results: Seq[NodeKey]) {
    override def toString = s"Edge(label=$label, args=${args.toList}, results=${results.toList})"
  }
}