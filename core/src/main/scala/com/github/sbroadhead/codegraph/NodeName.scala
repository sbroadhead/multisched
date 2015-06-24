package com.github.sbroadhead.codegraph

import scala.util._

/**
 * A [[CodeGraph]] node key, tagged with the node label type of that node.
 * @param key
 * @tparam V
 */
case class NodeName[+V](label: V, key: CodeGraph.NodeKey)

/**
 * Companion object for [[NodeName]].
 */
object NodeName {
  /**
   * Return a fresh node name of the specified type.
   * @tparam V The node label type.
   * @return A fresh node name.
   */
  def fresh[V](label: V): NodeName[V] = NodeName[V](label, Random.nextLong())

  implicit def extractNodeKey[N](n: NodeName[N]): CodeGraph.NodeKey = n.key
}
