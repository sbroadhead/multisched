package com.github.sbroadhead.multisched

/**
 * Container for `multisched` registers (node labels)
 */
object Registers {
  /** Base class for node labels */
  abstract class Register

  /** Vector of four word slots */
  case class VEC() extends Register
}
