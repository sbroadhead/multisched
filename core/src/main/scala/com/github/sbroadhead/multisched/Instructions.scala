package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._

/**
 * Container for `multisched` instruction definitions
 */
object Instructions {
  import Registers._

  abstract class Instruction

  /** Load constant value */
  case class const(value: (Int, Int, Int, Int)) extends Instruction with EdgeLabel.Nullary[VEC4]

  /** Add */
  case object `a` extends Instruction with EdgeLabel.Binary[VEC4, VEC4, /* -> */ VEC4]

  /** And */
  case object `and` extends Instruction with EdgeLabel.Binary[VEC4, VEC4, /* -> */ VEC4]

  /** Convert floating to signed integer */
  case class `cflts`(scale: Int) extends Instruction with EdgeLabel.Unary[VEC4, /* -> */ VEC4]

  /** Floating compare greater than */
  case object `fcgt` extends Instruction with EdgeLabel.Binary[VEC4, VEC4, /* -> */ VEC4]

  /** Floating multiply */
  case object `fm` extends Instruction with EdgeLabel.Binary[VEC4, VEC4, /* -> */ VEC4]

  /** Floating multiply and add */
  case object `fma` extends Instruction with EdgeLabel.Ternary[VEC4, VEC4, VEC4, /* -> */ VEC4]

  /** Rotate word immediate */
  case class `roti`(value: Int) extends Instruction with EdgeLabel.Unary[VEC4, /* -> */ VEC4]

  /** Select bits */
  case object `selb` extends Instruction with EdgeLabel.Ternary[VEC4, VEC4, VEC4, /* -> */ VEC4]

  /** Shuffle bytes */
  case object `shufb` extends Instruction with EdgeLabel.Ternary[VEC4, VEC4, VEC4, /* -> */ VEC4]

  //case object `clz`       extends EdgeLabel.Unary[]

  /*clz, cntb, fsm, fsmb, fsmh, gb, gbb, gbh, orx,
  frest, frsqest, fesd, frds, dfrest, dfrsqest
  :: VR n -> VR n

  rotqbi, rotqbybi, rotqmbi, rotqmbybi, shlqbi, shlqbybi, a, absdb, ah, and, andc, avgb,
  cbd, cbx, cdd, cdx, ceq, ceqb, ceqh, cgt, cgtb, cgth, chd,
  chx, clgt, clgtb, clgth, cwx, eqv, fa, fceq, fcgt,
  fcmeq, fcmgt, fm, fs, mpy, mpyh, mpyhh, mpyhhu, mpys, mpyu, nand, nor,
  or, orc, rot, roth, rothm, rotm, rotma, rotmah, rotqby,
  rotqmby, sf, sfh, shl, shlh, shlqby, sumb, xor, xorc,
  fi, dfm, dfa, dfs, dfcgt, dfceq, dfcmeq, dfcmgt, dcmpgedp
  :: VR n -> VR n -> VR n

  fma, fms, fnms, fnma, mpya, mradds, mpyhha, mpyhhau, selb, shufb, dfma, dfms, dfnma, dfnms
  :: VR n -> VR n -> VR n -> VR n

  fsmbi, il, ila, ilh, ilhu
  :: Integer -> VR n

  ahi, ai, andbi, andhi, andi, ceqbi, ceqhi, ceqi, cflts, cfltu,
  cgtbi, cgthi, cgti, clgtbi, cwd,
  clgthi, clgti, csflt, cuflt, mpyi, mpyui, orbi, orhi, ori, rothi, rothmi, roti,
  rotmahi, rotmai, rotmi, rotqbii, rotqbyi, rotqmbii, rotqmbyi, sfhi, sfi,
  shlhi, shli, shlqbii, shlqbyi, xorbi, xorhi, xori, iohl
  :: VR n -> Integer -> VR n

  undwrds, unwrds, unshorts, unbytes, unnibbles
  :: forall a . (Integral a) => [a] -> VR n

  dwrds, wrds, shorts, bytes, nibbles
  :: forall a . (Integral a) => VR n -> [a]

  unfloats, undoubles
  :: [Double] -> VR n

  floats, doubles
  :: VR n -> [Double]*/

}
