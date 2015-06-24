package com.github.sbroadhead.codegraph

/**
 * Exceptions relating to the code graph library.
 */
object Exceptions {
  case class InvalidEdgeKeyException(key: CodeGraph.EdgeKey)
    extends RuntimeException(s"The edge key '$key' was not found in the CodeGraph.")

  case class InvalidNodeKeyException(key: CodeGraph.NodeKey)
    extends RuntimeException(s"The node key '$key' was not found in the CodeGraph.")
}