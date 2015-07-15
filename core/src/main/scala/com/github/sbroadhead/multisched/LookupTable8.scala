package com.github.sbroadhead.multisched

import com.github.sbroadhead._
import codegraph._
import Registers._
import Instructions._

/**
 * Trait defining 8-way lookup tables.
 */
trait LookupTable8 {
  val builder: FunctionBuilder
  import builder.mutators._

  val values: Seq[Seq[Int]]

  /**
   * Perform a lookup in this table given the bit position of the low-order bit of the 3-bit
   * key embedded within `v`
   * @param low The low-order bit of the 3-bit key
   * @param v The value to extract the key from
   * @return The resulting column of the table
   */
  def lookup(low: Int)(v: NodeName[VEC4]): Seq[NodeName[VEC4]] = {
    def index(vt: (NodeName[VEC4], NodeName[VEC4])): NodeName[VEC4] = shufb.$(vt._1, vt._2, createKey(low, v))
    makeTable(low, values).map(index)
  }

  def ints(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int): Seq[Int] =
    Seq(a, b, c, d, e, f, g, h)

  def floats(a: Float, b: Float, c: Float, d: Float, e: Float, f: Float, g: Float, h: Float): Seq[Int] = {
    Seq(a, b, c, d, e, f, g, h).map(java.lang.Float.floatToIntBits)
  }

  private def createKey(low: Int, v: NodeName[VEC4]): NodeName[VEC4] = {
    val lowByte = low / 8
    val lowBit = low % 8
    val distance = (8 - lowBit) % 8
    val needRot = lowBit > 2
    val (byte, bit) = if (!needRot) (lowByte, lowBit) else (1 + lowByte, 0)
    val (c0123, mask) = bit match {
      case 0 => (splatInt4(0x00081018), splatInt4(0x07070707))
      case 1 => (splatInt4(0x00011011), splatInt4(0x0e0e0e0e))
      case 2 => (splatInt4(0x00010203), splatInt4(0x1c1c1c1c))
    }
    def splat(rotWidth: Int, x: NodeName[VEC4]) =
      shufb.$(x, x, bytes16(
        Seq(0, 0, 0, 0, 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12)
        .map { z => (z + ((3 - byte) % rotWidth)) % 16 }
      ))
    val look2 = if (!needRot) splat(16, v) else splat(4, roti(distance).$(v))
    selb.$(c0123, look2, mask)
  }

  private def makePair(low: Int)(xs: Seq[Int]): (NodeName[VEC4], NodeName[VEC4]) = {
    if (xs.length != 8) sys.error(s"Table row must be 8 words wide")

    val inOrderBytes = packInts(xs : _*)
    val order = low % 8 match {
      case 1 => for (j <- 0 to 7; i <- 0 to 1; k <- 0 to 1) yield (i, j, k)
      case 2 => for (j <- 0 to 7; i <- 0 to 3) yield (j, i, 0)
      case _ => for (j <- 0 to 7; i <- 0 to 3) yield (i, j, 0)
    }
    val permutedBytes = inOrderBytes.zip(order).sortBy(_._2).map(_._1.toInt)
    val (b1, b2) = permutedBytes.splitAt(16)
    (bytes16(b1), bytes16(b2))
  }

  private def makeTable(low: Int, xss: Seq[Seq[Int]]): Seq[(NodeName[VEC4], NodeName[VEC4])] =
    xss.map(makePair(low))
}