package com.github.sbroadhead

/**
 * Extension methods on [[CodeGraph]] for performing extended codegraph operations.
 * @param cg The underlying codegraph.
 * @tparam NKey Node key type.
 * @tparam EKey Edge key type.
 * @tparam N Node label type.
 * @tparam E Edge label type.
 */
class CodeGraphOps[NKey, EKey, N, E](cg: CodeGraph[NKey, EKey, N, E]) {

  /**
   * Return a proxy object for the given node key supporting extended operations on the node.
   * @param key The node key to wrap.
   * @throws CodeGraphOps.NodeNotFoundException if the node key is not in the codegraph.
   * @return A [[NodeOps]] instance wrapping the node given by `key`.
   */
  def node(key: NKey): NodeOps =
    if (cg.nodes.contains(key)) NodeOpsInternal(key)
    else throw CodeGraphOps.NodeNotFoundException(key)

  /**
   * Return a proxy object for the given edge key supporting extended operations on the edge.
   * @param key The edge key to wrap.
   * @throws CodeGraphOps.EdgeNotFoundException if the edge key is not in the codegraph.
   * @return An [[EdgeOps]] instance wrapping the edge given by `key`.
   */
  def edge(key: EKey): EdgeOps =
    if (cg.edges.contains(key)) EdgeOpsInternal(key)
    else throw CodeGraphOps.EdgeNotFoundException(key)

  /**
   * A wrapper for a node key, providing extended operations about the associated node.
   */
  sealed trait NodeOps {
    /** The underlying key. */
    val key: NKey

    /**
     * Returns the edge keys whose edge results contain this node.
     */
    def producers: Seq[EdgeOps] =
      cg.edges.filter { case (k, edge) => edge.results.contains(key) }.keys.map(EdgeOpsInternal).toSeq

    /**
     * Returns the edge keys whose edge inputs contain this node.
     */
    def consumers: Seq[EdgeOps] =
      cg.edges.filter { case (k, edge) => edge.args.contains(key) }.keys.map(EdgeOpsInternal).toSeq
  }

  /**
   * A wrapper for an edge key, providing extended operations about the associated edge.
   */
  sealed trait EdgeOps {
    /** The underlying key. */
    val key: EKey

    /**
     * Returns the edge keys whose edges produce results that are consumed, directly or not, by
     * this edge.
     * @note If the underlying codegraph has a cycle in it, this will result in an infinite loop.
     */
    def predecessors: Seq[EdgeOps] =
      cg.edges.get(key).toSeq.flatMap { edge =>
        edge.args.flatMap { node =>
          NodeOpsInternal(node).producers.flatMap { e => e.predecessors }
        }
      }

    /**
     * Returns the edge keys whose edges consume, directly or not, results that are produced by
     * this edge.
     * @note If the underlying codegraph has a cycle in it, this will result in an infinite loop.
     */
    def successors: Seq[EdgeOps] =
      cg.edges.get(key).toSeq.flatMap { edge =>
        edge.results.flatMap { node =>
          NodeOpsInternal(node).consumers.flatMap { e => e.predecessors }
        }
      }
  }

  private case class NodeOpsInternal(key: NKey) extends NodeOps

  private case class EdgeOpsInternal(key: EKey) extends EdgeOps

}

/**
 * Companion object for [[CodeGraphOps]].
 */
object CodeGraphOps {

  /**
   * Base class for [[CodeGraphOps]]-related exceptions.
   */
  sealed abstract class CodeGraphOpException extends RuntimeException

  /**
   * A given node key was not found in the associated code graph.
   * @param key The key that was not found.
   * @tparam K Key type.
   */
  case class NodeNotFoundException[K](key: K) extends CodeGraphOpException

  /**
   * A given edge key was not found in the associated code graph.
   * @param key The key that was not found.
   * @tparam K Key type.
   */
  case class EdgeNotFoundException[K](key: K) extends CodeGraphOpException

}