package com.github.sbroadhead.codegraph

import shapeless.{HNil, Generic}

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

  def nodeName[V](key: CodeGraph.NodeKey)(implicit gen: Generic.Aux[HNil, V]): NodeName[V] =
    NodeName(gen.to(HNil), key)

  implicit def extractNodeKey[N](n: NodeName[N]): CodeGraph.NodeKey = n.key
}
