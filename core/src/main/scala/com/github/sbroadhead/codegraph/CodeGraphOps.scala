package com.github.sbroadhead.codegraph

import scala.collection.mutable.{Map => MutableMap, MutableList, Set => MutableSet}
import scala.util.Random

/**
 * Extended operations on [[CodeGraph]].
 */
class CodeGraphOps[N, E](val cg: CodeGraph[N, E]) extends AnyVal {
  import Exceptions._

  /**
   * Dereference an edge key into its [[CodeGraph.Edge]] in this CodeGraph.
   * @throws Exceptions.InvalidEdgeKeyException if the key does not exist.
   * @param edges The edge keys to dereference.
   * @return The dereferenced edges.
   */
  def edge(edges: CodeGraph.EdgeKey*): Seq[CodeGraph.Edge[E]] =
    edges.map(x => cg.edges.getOrElse(x, throw InvalidEdgeKeyException(x)))

  /**
   * Dereference a node key into its node label in this CodeGraph.
   * @throws Exceptions.InvalidNodeKeyException if the key does not exist.
   * @param nodes The node keys to dereference.
   * @return The dereferenced node labels.
   */
  def node(nodes: CodeGraph.NodeKey*): Seq[N] =
    nodes.map(x => cg.nodes.getOrElse(x, throw InvalidNodeKeyException(x)))

  /**
   * Return a sequence of edge keys representing the producing edges
   * of a given node.
   * @param node the node to query.
   * @return The producing edge keys.
   */
  def producersOf(node: CodeGraph.NodeKey): Seq[CodeGraph.EdgeKey] =
    cg.edges.collect {
      case (key, edge) if edge.results.contains(node) => key
    }.toSeq

  /**
   * Return a sequence of edge keys representing the consuming edges
   * of a given node.
   * @param node the node to query.
   * @return The consuming edge keys.
   */
  def consumersOf(node: CodeGraph.NodeKey): Seq[CodeGraph.EdgeKey] =
    cg.edges.collect {
      case (key, edge) if edge.args.contains(node) => key
    }.toSeq

  /**
   * Return a sequence of edge keys in topologically sorted order.
   * @return a sequence of edge keys.
   */
  def topSort: Seq[CodeGraph.EdgeKey] = {
    val nodes: MutableSet[CodeGraph.NodeKey] = MutableSet(cg.inputs : _*)
    val edgesLeft: MutableMap[CodeGraph.EdgeKey, CodeGraph.Edge[E]] = MutableMap(cg.edges.toSeq : _*)
    val edges: MutableList[CodeGraph.EdgeKey] = MutableList()
    while (edgesLeft.nonEmpty) {
      val newEdges = edgesLeft.filter { case (k, e) => e.args.forall(nodes.contains) }
      if (newEdges.isEmpty) {
        throw TopSortFailedException()
      }
      edgesLeft --= newEdges.keys
      edges ++= newEdges.keys.toSeq
      nodes ++= newEdges.values.flatMap(e => e.results)
    }
    edges
  }

  /**
   * Return a new CodeGraph with the same structure as this one, but with new node names,
   * suitable for splicing into an existing CodeGraph.
   * @return
   */
  def duplicate: CodeGraph[N, E]  = {
    val renameMap = cg.nodes.keys.zip(Seq.fill(cg.nodes.size)(Random.nextLong())).toMap
    def rename(edge: CodeGraph.Edge[E]): CodeGraph.Edge[E] =
      new CodeGraph.Edge[E](
        edge.label,
        edge.args.map(x => renameMap.getOrElse(x, sys.error("impossible: node not found"))),
        edge.results.map(x => renameMap.getOrElse(x, sys.error("impossible: node not found"))))

    val newNodes = cg.nodes.map { case (k, v) => (renameMap.getOrElse(k, sys.error("impossible: node not found")), v) }
    val newEdges = cg.edges.map { case (k, v) => (k, rename(v)) }
    val newInputs = cg.inputs.map { x => renameMap.getOrElse(x, sys.error("impossible: node not found")) }
    val newOutputs = cg.outputs.map { x => renameMap.getOrElse(x, sys.error("impossible: node not found")) }

    CodeGraph[N, E](newNodes, newEdges, newInputs, newOutputs)
  }
}

/**
 * Companion object for [[CodeGraphOps]].
 */
object CodeGraphOps {
  /**
   * Implicit conversion from [[CodeGraph]] to [[CodeGraphOps]].
   * @param cg The inner [[CodeGraph]].
   * @return The wrapper [[CodeGraphOps]].
   */
  implicit def fromCodeGraph[N, E](cg: CodeGraph[N, E]): CodeGraphOps[N, E] =
    new CodeGraphOps(cg)
}