package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._
import Instructions._
import Registers._

/**
 * Builder for `multisched` functions.
 */
class FunctionBuilder extends CodeGraphBuilder[Register, Instruction] {
  /**
   * The function `2^x` on integers.
   */
  def exp2(p: Int): Int = p match {
    case 0 => 1
    case x if x > 0 => 2 * exp2(p - 1)
    case x if x < 0 => exp2(p + 1) / 2
  }

  /**
   * Turns a float into a constant vector of the same float repeated four times.
   */
  def splatFloat4(f: Float): NodeName[VEC4[FLOAT]] =
    mutators.EdgeFactory(Instructions.const[(Float, Float, Float, Float), VEC4[FLOAT]]((f, f, f, f))).$()

  /**
   * Turns an integer into a constant vector of the same integer repeated four times.
   */
  def splatInt4(w: Int): NodeName[VEC4[INT]] =
    mutators.EdgeFactory(Instructions.const[(Int, Int, Int, Int), VEC4[INT]]((w, w, w, w))).$()
}
