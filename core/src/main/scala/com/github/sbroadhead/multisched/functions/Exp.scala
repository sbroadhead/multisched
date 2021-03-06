package com.github.sbroadhead.multisched.functions

import com.github.sbroadhead.codegraph._
import com.github.sbroadhead.multisched._

/**
 * Exponential function.
 */
object Exp {
  import CodeGraphOps._
  import Instructions._
  import Registers._

  object exp extends FunctionBuilder.Typed[Unary[VEC], Unary[VEC]] {
    object vec extends VecUtils(this)
    object math extends MathUtils(this)

    val v = input

    val vByLog2 = fm(v, vec.splatFloat4({ 1 / scala.math.log(2) * (1 + 1.5 * exp2f(-24)) }.toFloat))

    val restrictDomainMin = fcgt(vec.splatFloat4(-127), vByLog2)
    val domainMin = selb(vByLog2, vec.splatFloat4(-127), restrictDomainMin)
    val restrictDomainMax = fcgt(vByLog2, vec.splatFloat4(129 - 128 * exp2f(-23)))

    val vByLog2AsInt = cflts(23)(domainMin)
    val exponent = and(vByLog2AsInt, vec.splatInt4(0xff800000))
    val exp = a(exponent, vec.splatInt4(0x3f800000))
    val frac = math.onePlusMant(20, vByLog2AsInt)
    val coeffs = coeffTable.lookup(20)(vByLog2AsInt).zipWithIndex.map { case (n, i) => n named s"coeff$i" }
    val polyResult = math.evalPolynomial(coeffs)(frac)
    val raw = fm(exp, polyResult)
    val result = selb(raw, vec.splatFloat4(Float.MaxValue), restrictDomainMax)

    output(result)

    object coeffTable extends LookupTable8(this) {
      override val values = Seq(
        floats(
          0.488910472505717946544333995964f, 0.533160650848510271418706237762f, 0.581415812503142018228284047676f,
          0.634038439428529967623811116225f, 0.691423821003824519148965598205f, 0.754003023353629632384100467241f,
          0.822246127420115970730423861595f, 0.896665760105699212437748621431f),
        floats(
          0.386996678257891908889515995259f, 0.422022870155999920962990896473f, 0.460219203266703906733335040636f,
          0.501872599883384618334631701530f, 0.547295950985647815642341785321f, 0.596830466606234787233071184109f,
          0.650848238924312874042930509462f, 0.709755037338528327856751565010f),
        floats(
          0.0661232417618669572168933408550f, 0.0721079064502102126340139082810f, 0.0786342295702572469581413173390f,
          0.0857512353985405885343425226400f, 0.0935123852877072794478736750250f, 0.101975979256217663333955160213f,
          0.111205593925017272195985227523f, 0.121270560090863936668552711023f),
        floats(
          0.0579696479196762013464425152008f, 0.0632163493162893647847828263023f, 0.0689379177602816264169415242142f,
          0.0751773323914287139050205811200f, 0.0819814622939993592123900582478f, 0.0894014185668115541282306153820f,
          0.0974929382583513368093151815639f, 0.106316803050988670258058888136f)
      )
    }
  }

  object outer extends FunctionBuilder.Typed[Unary[VEC], Unary[VEC]] {
    val v = input
    val expV = nest(exp)(v)
    output(expV)
  }

  val nodeNameMap: Map[CodeGraph.NodeKey, String] = exp.nodeNames
}