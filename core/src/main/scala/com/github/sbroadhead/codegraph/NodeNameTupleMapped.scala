package com.github.sbroadhead.codegraph

import shapeless.ops.{hlist => hl}
import shapeless._

/**
 * Type class witnessing that the result of wrapping each element of tuple `L` in type constructor `NodeName` is `Out`.
 *
 * For example, `NodeNameTupleMapped[(INT, BOOL)].Out` should be `(NodeName[INT], NodeName[BOOL])`.
 */
trait NodeNameTupleMapped[L] extends Serializable { type Out <: Product }

object NodeNameTupleMapped {
  type Aux[L, Out0] = NodeNameTupleMapped[L] { type Out = Out0 }

  implicit def nodeNamePUnitMapper: NodeNameTupleMapped.Aux[PUnit, PUnit] =
    new NodeNameTupleMapped[PUnit] {
      type Out = PUnit
    }

  implicit def nodeNameTupleMapper[Args <: Product, AGen <: HList, AMap <: HList, ATup <: Product]
    (implicit
      agen: Generic.Aux[Args, AGen],
      amap: hl.Mapped.Aux[AGen, NodeName, AMap],
      atup: hl.Tupler.Aux[AMap, ATup]): NodeNameTupleMapped.Aux[Args, ATup] =
        new NodeNameTupleMapped[Args] {
          type Out = ATup
        }
}