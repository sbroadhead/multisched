package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._

/**
 * Container for multisched instruction definitions
 */
object Instructions {
  import Registers._

  abstract class Instruction

  object Sig {
    trait Nullary extends Instruction with EdgeLabel.Nullary[VEC]
    trait Unary extends Instruction with EdgeLabel.Unary[VEC, VEC]
    trait Binary extends Instruction with EdgeLabel.Binary[VEC, VEC, VEC]
    trait Ternary extends Instruction with EdgeLabel.Ternary[VEC, VEC, VEC, VEC]
  }

  /** Load constant value */
  case class const(value: Vec4) extends Sig.Nullary

  /** Add */
  case object a extends Sig.Binary

  /** And */
  case object and extends Sig.Binary

  /** Convert floating to signed integer */
  case class cflts(scale: Int) extends Sig.Unary

  /** Floating compare greater than */
  case object fcgt extends Sig.Binary

  /** Floating multiply */
  case object fm extends Sig.Binary

  /** Floating multiply and add */
  case object fma extends Sig.Ternary

  /** Rotate word immediate */
  case class roti(value: Int) extends Sig.Unary

  /** Select bits */
  case object selb extends Sig.Ternary

  /** Shuffle bytes */
  case object shufb extends Sig.Ternary

  case class nest[Input <: Product, Output <: Product, N <: Register, E <: Instruction]
    (cg: CodeGraph[N, E] with CodeGraphInterface.Aux[Input, Output])
    extends Instruction with EdgeLabel[Input, Output]
}
