package com.github.sbroadhead.codegraph

import com.github.sbroadhead._

import scala.collection.mutable
import scala.language.experimental.macros
import scala.collection.mutable.{Seq => MutableSeq}
import shapeless._
import shapeless.ops.{hlist => hl, tuple => tp}

/**
 * Trait supporting mutable [[CodeGraph]] operations.
 * @tparam N The node label type.
 * @tparam E The edge label type.
 */
trait CodeGraphBuilder[N, E] extends CodeGraph[N, E] {
  /** The [[MutableCodeGraph]] underlying this instance. */
  implicit val cg: MutableCodeGraph[N, E] = new MutableCodeGraph[N, E]

  def nodes = cg.nodes.toMap
  def edges = cg.edges.toMap
  def inputs = cg.inputs.toSeq
  def outputs = cg.outputs.toSeq

  // Inside the code graph builder, 1-tuples are convertible to their container types
  protected implicit def goAwayTuple1[T](tup: Tuple1[T]): T = tup._1
}
