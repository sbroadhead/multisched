package com.github.sbroadhead

import scala.collection.mutable.{Map => MMap, Seq => MSeq}

/**
 * Supports building a [[CodeGraph]] in-place using mutable data structures.
 * @tparam NKey The key type for indirectly storing nodes. Must have an implicit [[Generator]] for creating new keys.
 * @tparam EKey The key type for indirectly storing edges. Must have an implicit [[Generator]] for creating new keys.
 * @tparam N The node label type.
 * @tparam K
 */
class CodeGraphBuilder[NKey: Generator, EKey: Generator, N, K] extends MutableCodeGraph[NKey, EKey, N, K] {
  private var _nodes: MMap[NKey, N] = MMap()
  private var _edges: MMap[EKey, Edge] = MMap()
  private var _outputs: MSeq[NKey] = MSeq()
  private var _inputs: MSeq[NKey] = MSeq()

  /**
   * Add a node to this codegraph.
   * @param n The label of the new node.
   * @return The key of the new node.
   */
  override def addNode(n: N): NKey = {
    val key = generator[NKey].generate
    _nodes += key -> n
    key
  }

  /**
   * Add an edge to this codegraph.
   * @param e The label of the new node.
   * @param args The argument node keys.
   * @param results The result node keys.
   * @throws CodeGraphBuilder.InvalidNodeKeysException if non-existant node keys are referenced.
   * @return The key of the new edge.
   */
  override def addEdge(e: K, args: Seq[NKey], results: Seq[NKey]): EKey = {
    if ((args ++ results).forall(nodes.contains)) {
      val key = generator[EKey].generate
      val edge = Edge(e, args, results)
      _edges += key -> edge
      key
    } else {
      throw CodeGraphBuilder.InvalidNodeKeysException((args ++ results).filter(x => !nodes.contains(x)))
    }
  }

  /**
   * Returns a mapping from node keys to their labels.
   * @return The map.
   */
  override def nodes: Map[NKey, N] = _nodes.toMap

  /**
   * Returns a mapping from edge keys to the underlying edge structure.
   * @return The map.
   */
  override def edges: Map[EKey, Edge] = _edges.toMap

  /**
   * Returns the node keys that represent outputs of this codegraph.
   * @return The node keys.
   */
  override def outputs: Seq[NKey] = _outputs

  /**
   * Set the outputs of this codegraph.
   * @param out The outputs.
   */
  override def outputs_=(out: Seq[NKey]): Unit = {
    _outputs = MSeq(out: _*)
  }

  /**
   * Returns the node keys that represent inputs to this codegraph.
   * @return The node keys.
   */
  override def inputs: Seq[NKey] = _inputs

  /**
   * Set the inputs of this codegraph.
   * @param in The inputs.
   */
  override def inputs_=(in: Seq[NKey]): Unit = {
    _inputs = MSeq(in: _*)
  }
}

/**
 * Companion object to [[CodeGraphBuilder]].
 */
object CodeGraphBuilder {

  /**
   * Base class for all [[CodeGraphBuilder]]-related exceptions.
   */
  abstract class CodeGraphBuilderException extends RuntimeException

  /**
   * Invalid node keys were referenced in a [[CodeGraphBuilder]] operation.
   * @param keys The node keys.
   * @tparam NKey The node key type.
   */
  case class InvalidNodeKeysException[NKey](keys: Seq[NKey])
    extends RuntimeException("Some nodes were referenced that do not exist in the codegraph.")

}