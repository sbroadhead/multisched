package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._

/**
 * Container for `multisched` instruction definitions
 */
object Instructions {
  import Registers._

  abstract class Instruction

  case class const[T, N](value: T)(implicit val c: Const.Aux[T, N]) extends Instruction with EdgeLabel.Nullary[N]

  /** Floating multiply. */
  case object `fm` extends Instruction with EdgeLabel.Binary[VEC4[FLOAT], VEC4[FLOAT], /* -> */ VEC4[FLOAT]]

  /** Convert floating to signed integer */
  case class `cflts`(scale: Int) extends Instruction with EdgeLabel.Unary[VEC4[FLOAT], /* -> */ VEC4[INT]]

  /** And */
  case object `and` extends Instruction with EdgeLabel.Binary[VEC4[INT], VEC4[INT], /* -> */ VEC4[INT]]

  /** Add */
  case object `a` extends Instruction with EdgeLabel.Binary[VEC4[INT], VEC4[INT], /* -> */ VEC4[INT]]

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
