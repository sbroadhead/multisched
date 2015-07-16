package com.github.sbroadhead.multisched

import java.lang.{Float => F}

/**
 * Convenience wrapper for a vector of four 32-bit words.
 */
case class Vec4(a: Int, b: Int, c: Int, d: Int) { self =>
  def this(splat: Int) = this(splat, splat, splat, splat)

  def this(splat: Float) = this(F.floatToIntBits(splat))

  def this(a: Float, b: Float, c: Float, d: Float) =
    this(F.floatToIntBits(a), F.floatToIntBits(b), F.floatToIntBits(c), F.floatToIntBits(d))

  def this(bytes: Seq[Byte]) =
      this(
        bytes.slice(0, 4).foldLeft(0)(_ << 8 | _.toInt & 0xff),
        bytes.slice(4, 8).foldLeft(0)(_ << 8 | _.toInt & 0xff),
        bytes.slice(8, 12).foldLeft(0)(_ << 8 | _.toInt & 0xff),
        bytes.slice(12, 16).foldLeft(0)(_ << 8 | _.toInt & 0xff))

  def words: Seq[Int] = Seq(a, b, c, d)

  def map(fn: Int => Int) = new Vec4(fn(a), fn(b), fn(c), fn(d))

  def zipWith(fn: (Int, Int) => Int)(other: Vec4) =
    new Vec4(fn(a, other.a), fn(b, other.b), fn(c, other.c), fn(d, other.d))

  def tuple: (Int, Int, Int, Int) = (a, b, c, d)

  /**
   * View of this object as floats rather than words.
   */
  object floatView {
    val a = F.intBitsToFloat(self.a)
    val b = F.intBitsToFloat(self.b)
    val c = F.intBitsToFloat(self.c)
    val d = F.intBitsToFloat(self.d)

    def floats: Seq[Float] = Seq(a, b, c, d)

    def map(fn: Float => Float) = new Vec4(fn(a), fn(b), fn(c), fn(d))

    def zipWith(fn: (Float, Float) => Float)(other: Vec4) =
      new Vec4(fn(a, other.floatView.a), fn(b, other.floatView.b), fn(c, other.floatView.c), fn(d, other.floatView.d))

    def tuple: (Float, Float, Float, Float) = (a, b, c, d)
  }

  /**
   * View of this object as bytes rather than words.
   */
  object byteView {
    def bytes = packInts(a, b, c, d)

    def map(fn: Byte => Byte) = new Vec4(bytes.map(fn))

    def zipWith(fn: (Byte, Byte) => Byte)(other: Vec4) =
      new Vec4(bytes.zip(other.byteView.bytes).map(fn.tupled))
  }
}
