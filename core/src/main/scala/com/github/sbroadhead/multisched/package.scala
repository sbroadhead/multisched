package com.github.sbroadhead

import com.github.sbroadhead._
import codegraph._

package object multisched {
  import Instructions._
  import Registers._

  trait FunctionGraph extends CodeGraph[Register, Instruction] with CodeGraphInterface
  
  /**
   * The function `2^x`.
   */
  def exp2f(p: Float): Float = p match {
    case 0 => 1
    case x if x > 0 => 2 * exp2f(p - 1)
    case x if x < 0 => exp2f(p + 1) / 2
  }

  /**
   * The function `2^x`.
   */
  def exp2(p: Int): Int = p match {
    case 0 => 1
    case x if x > 0 => 2 * exp2(p - 1)
    case x if x < 0 => exp2(p + 1) / 2
  }

  /**
   * Pack floats into an array of bytes.
   * @param flts The floats to pack.
   * @return An array of bytes.
   */
  def packFloats(flts: Float*): Array[Byte] = {
    val bb = java.nio.ByteBuffer.allocate(flts.size * 4)
    for (f <- flts) bb.putFloat(f)
    bb.array
  }

  /**
   * Pack ints into an array of bytes.
   * @param ints The ints to pack.
   * @return An array of bytes.
   */
  def packInts(ints: Int*): Array[Byte] = {
    val bb = java.nio.ByteBuffer.allocate(ints.size * 4)
    for (f <- ints) bb.putInt(f)
    bb.array
  }
}
