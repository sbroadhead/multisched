package com.github.sbroadhead.multisched.functions

import com.github.sbroadhead.codegraph._
import com.github.sbroadhead.multisched._

/**
 * Exponential function
 */
class Exp extends FunctionBuilder {
  import Instructions._
  import Registers._
  import mutators._

  type Input = Tuple1[VEC4[FLOAT]]
  type Output = Tuple1[VEC4[FLOAT]]

  val v = input
  //(unfloats4 (1/log(2)*(1+1.5*2**(-24))))
  val vByLog2 = fm.$(v, splatFloat4({ 1 / math.log(2) * (1 + 1.5 * exp2(-24)) }.toFloat))
  val vByLog2AsInt = cflts(23).$(vByLog2)
  val exponent = and.$(vByLog2AsInt, splatInt4(0xff800000))
  val exp = a.$(exponent, splatInt4(0x3f800000))
  
}
