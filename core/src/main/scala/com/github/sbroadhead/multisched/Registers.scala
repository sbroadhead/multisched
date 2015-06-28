package com.github.sbroadhead.multisched

/**
 * Container for `multisched` registers (node labels)
 */
object Registers {
  /** Base class for node labels */
  abstract class Register

  /** Integer */
  case class INT() extends Register

  /** Boolean */
  case class BOOL() extends Register

  /** Float */
  case class FLOAT() extends Register

  /** Vector of another type of node */
  case class VEC4[T <: Register]() extends Register

  /**
   * A trait supporting promotion of a constant to its underlying register type.
   * @tparam T The type of the register to define a constant type for.
   */
  trait Const[T] {
    type ValueType = T
    type NodeType <: Register
    def promote(value: ValueType) = Instructions.const[ValueType, NodeType](value)(this)
  }

  object Const {
    type Aux[T, NodeType0] = Const[T] { type NodeType = NodeType0 }
  }

  implicit val intConst = new Const[Int] { type NodeType = INT }
  implicit val boolConst = new Const[Boolean] { type NodeType = BOOL }
  implicit val floatConst = new Const[Float] { type NodeType = FLOAT }
  implicit def vec4Const[T](implicit constInner: Const[T]) = new Const[Tuple4[T, T, T, T]] {
    type NodeType = VEC4[constInner.NodeType]
  }
}
