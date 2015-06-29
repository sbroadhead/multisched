package com.github.sbroadhead.multisched

import com.github.sbroadhead._
import codegraph._
import multisched._

/**
 * Base trait for math utility functions.
 */
trait MathUtils {
  val builder: FunctionBuilder

  import Instructions._
  import Registers._

  /**
   * Returns `1.m` where `m` is `2^bits-1`.
   */
  def onePlusMant(bits: Int, v: NodeName[VEC4]): NodeName[VEC4] = {
    import builder.mutators._
    selb.$(splatInt4(0x3f800000), v, splatFloat4(exp2(bits.toFloat) - 1))._1
  }

  /**
   * Evaluates a polynomial.
   * @param coeffs The coefficient nodes.
   * @param v The value at which to evaluate the polynomial.
   * @return The value of the evaluated polynomial.
   */
  def evalPolynomial(coeffs: Seq[NodeName[VEC4]])(v: NodeName[VEC4]): NodeName[VEC4] = {
    import builder.mutators._
    var r = coeffs.head
    for (c <- coeffs.tail) {
      r = fma.$(r, v, c)
    }
    r
  }
}
