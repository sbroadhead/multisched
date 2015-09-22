package com.github.sbroadhead.codegraph

import shapeless._

/**
 * Trait for unwrapping Tuple1[T] into T, and leaving all other types alone.
 */
trait TupleUnwrapper[T] extends DepFn1[T] with Serializable { type Out }

trait TupleUnwrapperBase {
  implicit def tuplePassthrough[T]: TupleUnwrapper.Aux[T, T] =
    new TupleUnwrapper[T] {
      type Out = T
      override def apply(t: T): T = t
    }
}

object TupleUnwrapper extends TupleUnwrapperBase {
  def apply[T](implicit inst: TupleUnwrapper[T]): TupleUnwrapper[T] = inst

  type Aux[T, Out0] = TupleUnwrapper[T] { type Out = Out0 }

  implicit def tupleUnwrapper[T]: TupleUnwrapper.Aux[Tuple1[T], T] =
    new TupleUnwrapper[Tuple1[T]] {
      type Out = T
      override def apply(t: Tuple1[T]): T = t._1
    }
}
