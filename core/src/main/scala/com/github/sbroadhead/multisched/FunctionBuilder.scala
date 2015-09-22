package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._
import Instructions._
import Registers._

/**
 * Builder for `multisched` functions.
 */
trait FunctionBuilder
  extends FunctionGraph
  with CodeGraphBuilder[Register, Instruction]

object FunctionBuilder {
  trait Typed[Input0 <: Product, Output0 <: Product] extends FunctionBuilder {
    type Input = Input0
    type Output = Output0
  }
}
