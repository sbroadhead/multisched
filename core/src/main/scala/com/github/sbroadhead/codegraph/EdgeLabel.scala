package com.github.sbroadhead.codegraph

import shapeless._

/**
 * Base trait for edge label types.
 * @tparam Args0 tuple of argument types.
 * @tparam Results0 tuple of result types.
 */
trait EdgeLabel[Args0 <: Product, Results0 <: Product] {
  type Args = Args0
  type Results = Results0
}

/**
 * Companion object for [[EdgeLabel]].
 */
object EdgeLabel {
  /** Nullary operation of the form `() => R`. */
  type Nullary[R] = EdgeLabel[PUnit, Tuple1[R]]

  /** Unary operation of the form `A => R`. */
  type Unary[A, R] = EdgeLabel[Tuple1[A], Tuple1[R]]

  /** Binary operation of the form `A * B => R`. */
  type Binary[A, B, R] = EdgeLabel[(A, B), Tuple1[R]]

  /** Ternary operation of the form `A * B * C => R`. */
  type Ternary[A, B, C, R] = EdgeLabel[(A, B, C), Tuple1[R]]

  /** Unary operation of the form `A => R * S`. */
  type Unary2[A, R, S] = EdgeLabel[Tuple1[A], (R, S)]

  /** Binary operation of the form `A * B => R * S`. */
  type Binary2[A, B, R, S] = EdgeLabel[(A, B), (R, S)]

  /** Ternary operation of the form `A * B * C => R * S`. */
  type Ternary2[A, B, C, R, S] = EdgeLabel[(A, B, C), (R, S)]
}