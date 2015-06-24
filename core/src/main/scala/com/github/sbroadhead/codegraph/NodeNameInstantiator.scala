package com.github.sbroadhead.codegraph

import shapeless._
import shapeless.ops.{hlist => hl}

/**
 * Trait for instantiating a tuple type (A, B, C, ...) into
 * ([[NodeName]]`[A]`, [[NodeName]]`[B]`, [[NodeName]]`[C]`) with fresh node names.
 */
trait NodeNameInstantiator[T] extends DepFn0 with Serializable { type Out <: Product }

object NodeNameInstantiator {
  def apply[T](implicit inst: NodeNameInstantiator[T]): NodeNameInstantiator[T] = inst

  type Aux[T, Out0] = NodeNameInstantiator[T] { type Out = Out0 }

  implicit def nodeNamePUnitInstantiator: NodeNameInstantiator.Aux[PUnit, PUnit] =
    new NodeNameInstantiator[PUnit] {
      type Out = PUnit
      override def apply(): PUnit = <>
    }

  implicit def nodeNameInstantiator[Args <: Product, AGen <: HList, AInst <: HList, ATup <: Product]
    (implicit
      agen: Generic.Aux[Args, AGen],
      inst: NodeNameHListInstantiator.Aux[AGen, AInst],
      atup: hl.Tupler.Aux[AInst, ATup]): NodeNameInstantiator.Aux[Args, ATup] =
        new NodeNameInstantiator[Args] {
          type Out = ATup
          override def apply(): ATup = atup(inst.apply())
        }
}

/**
 * Auxilliary trait used by [[NodeNameInstantiator]].
 */
trait NodeNameHListInstantiator[L <: HList] extends DepFn0 with Serializable

object NodeNameHListInstantiator {
  type Aux[L <: HList, Out0] = NodeNameHListInstantiator[L] { type Out = Out0 }

  implicit def instHNil: NodeNameHListInstantiator.Aux[HNil, HNil] =
    new NodeNameHListInstantiator[HNil] {
      type Out = HNil
      override def apply() = HNil
    }

  implicit def instHList[H, T <: HList, TN <: HList]
  (implicit
    hgen: Generic.Aux[H, HNil],
    inst: NodeNameHListInstantiator.Aux[T, TN]): NodeNameHListInstantiator.Aux[H :: T, NodeName[H] :: TN] =
      new NodeNameHListInstantiator[H :: T] {
        type Out = NodeName[H] :: TN
        override def apply(): NodeName[H] :: TN = NodeName.fresh[H](hgen.from(HNil)) :: inst.apply()
      }
}