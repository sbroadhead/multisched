package com.github.sbroadhead

import shapeless._
import shapeless.ops.hlist.Tupler
import shapeless.ops.{hlist => hl}

package object codegraph {
  /** Stand-in for `Unit`, but as a case class so it implements `Product`. */
  case class PUnit()

  /** [[PUnit]] version of () */
  def <> = PUnit()

  /** For the auto-instantiator behavior in [[NodeNameInstantiator]]. */
  implicit def hnilTuplesToPUnitToo: hl.Tupler.Aux[HNil, PUnit] =
    new Tupler[HNil] {
      type Out = PUnit
      override def apply(t: HNil): PUnit = <>
    }

}
