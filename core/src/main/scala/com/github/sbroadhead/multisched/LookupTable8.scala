package com.github.sbroadhead.multisched

import com.github.sbroadhead._
import codegraph._
import Registers._
import Instructions._

/**
 * Trait defining 8-way lookup tables.
 */
abstract class LookupTable8(builder: FunctionBuilder) {
  val values: Seq[Seq[Int]]

  import builder._

  object math extends MathUtils(builder)
  object vec extends VecUtils(builder)

  /**
   * Perform a lookup in this table given the bit position of the low-order bit of the 3-bit
   * key embedded within `v`
   * @param low The low-order bit of the 3-bit key
   * @param v The value to extract the key from
   * @return The resulting column of the table
   */
  def lookup(low: Int)(v: NodeName[VEC]): Seq[NodeName[VEC]] = {
    def index(vt: (NodeName[VEC], NodeName[VEC])): NodeName[VEC] = shufb(vt._1, vt._2, createKey(low, v)).named("lookupIndex")
    makeTable(low, values).map(index)
  }

  def ints(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int): Seq[Int] =
    Seq(a, b, c, d, e, f, g, h)

  def floats(a: Float, b: Float, c: Float, d: Float, e: Float, f: Float, g: Float, h: Float): Seq[Int] = {
    Seq(a, b, c, d, e, f, g, h).map(java.lang.Float.floatToIntBits)
  }

  private def createKey(low: Int, v: NodeName[VEC]): NodeName[VEC] = {
    val lowByte = low / 8
    val lowBit = low % 8
    val distance = (8 - lowBit) % 8
    val needRot = lowBit > 2
    val (byte, bit) = if (!needRot) (lowByte, lowBit) else (1 + lowByte, 0)
    val (c0123, mask) = bit match {
      case 0 => (vec.splatInt4(0x00081018).named("c0123"), vec.splatInt4(0x07070707).named("mask"))
      case 1 => (vec.splatInt4(0x00011011).named("c0123"), vec.splatInt4(0x0e0e0e0e).named("mask"))
      case 2 => (vec.splatInt4(0x00010203).named("c0123"), vec.splatInt4(0x1c1c1c1c).named("mask"))
    }
    def splat(rotWidth: Int, x: NodeName[VEC]) =
      shufb(x, x, vec.bytes16(
        Seq(0, 0, 0, 0, 4, 4, 4, 4, 8, 8, 8, 8, 12, 12, 12, 12)
        .map { z => (z + ((3 - byte) % rotWidth)) % 16 }
      ).named("splatKey")).named(s"splat($rotWidth)")
    val look2 = { if (!needRot) splat(16, v) else splat(4, roti(distance)(v)) }.named("look2")
    giveName(selb(c0123, look2, mask), s"createKey")
  }

  private def makePair(low: Int)(xs: Seq[Int]): (NodeName[VEC], NodeName[VEC]) = {
    if (xs.length != 8) sys.error(s"Table row must be 8 words wide")

    val inOrderBytes = packInts(xs : _*)
    val order = low % 8 match {
      case 1 => for (j <- 0 to 7; i <- 0 to 1; k <- 0 to 1) yield (i, j, k)
      case 2 => for (j <- 0 to 7; i <- 0 to 3) yield (j, i, 0)
      case _ => for (j <- 0 to 7; i <- 0 to 3) yield (i, j, 0)
    }
    val permutedBytes = inOrderBytes.zip(order).sortBy(_._2).map(_._1.toInt)
    val (b1, b2) = permutedBytes.splitAt(16)
    (vec.bytes16(b1).named("makePair_1"), vec.bytes16(b2).named("makePair_2"))
  }

  private def makeTable(low: Int, xss: Seq[Seq[Int]]): Seq[(NodeName[VEC], NodeName[VEC])] =
    xss.map(makePair(low))
}