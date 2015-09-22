package com.github.sbroadhead.multisched

import Instructions._
import Registers._
import com.github.sbroadhead.codegraph._

class VecUtils(builder: FunctionBuilder) {
  import builder._

  def splatFloat4(f: Float): NodeName[VEC] = {
    val i = java.lang.Float.floatToRawIntBits(f)
    const(new Vec4(i))()
  }

  def splatInt4(w: Int): NodeName[VEC] =
    const(new Vec4(w))()

  def bytes16(b: Seq[Int]): NodeName[VEC] = {
    if (b.length != 16)
      throw new RuntimeException("bytes16 requires a 16-byte argument")
    val Seq(p, q, r, s): Seq[Int] = b.grouped(4).map(_.fold(0)((x, y) => x << 8 | (y & 0xff))).toSeq
    const(new Vec4(p, q, r, s))()
  }
}
