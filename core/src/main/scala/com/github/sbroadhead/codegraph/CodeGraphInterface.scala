package com.github.sbroadhead.codegraph

import com.github.sbroadhead.codegraph.CodeGraph.NodeKey

/**
 * A trait for specifying the interface of a [[CodeGraph]] by giving the types
 * of its arguments and outputs.
 */
trait CodeGraphInterface {
  type Input <: Product
  type Output <: Product
}

/**
 * Companion object for [[CodeGraphInterface]].
 */
object CodeGraphInterface {
  type Aux[Input0, Output0] = CodeGraphInterface {
    type Input = Input0
    type Output = Output0
  }
}
