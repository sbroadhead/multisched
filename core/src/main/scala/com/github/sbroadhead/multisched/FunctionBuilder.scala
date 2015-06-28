package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._
import Instructions._
import Registers._

/**
 * Builder for `multisched` functions.
 */
class FunctionBuilder extends CodeGraphBuilder[Register, Instruction] { self =>
  trait FunctionBuilderMutators extends DefaultMutators {
    /**
     * Turns a float into a constant vector of the same float repeated four times.
     */
    def splatFloat4(f: Float): NodeName[VEC4] = {
      val i = java.lang.Float.floatToRawIntBits(f)
      Instructions.const((i, i, i, i)).$()
    }

    /**
     * Turns an integer into a constant vector of the same integer repeated four times.
     */
    def splatInt4(w: Int): NodeName[VEC4] =
      Instructions.const((w, w, w, w)).$()

    /**
     * Turns a sequence of 16 bytes into a constant vector.
     */
    def bytes16(b: Seq[Int]): NodeName[VEC4] = {
      if (b.length != 16)
        throw new RuntimeException("bytes16 requires a 16-byte argument")
      val Seq(p, q, r, s): Seq[Int] = b.grouped(4).map(_.fold(0)(_ << 8 | _)).toSeq
      Instructions.const((p, q, r, s)).$()
    }
  }

  /** Specialized function builder mutators */
  override val mutators = new FunctionBuilderMutators {}

  /* Math utilities */
  val mathUtils = new MathUtils {
    override val builder: FunctionBuilder = self
  }
}
