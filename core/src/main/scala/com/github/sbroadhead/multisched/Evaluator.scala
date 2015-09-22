package com.github.sbroadhead.multisched

import com.github.sbroadhead.codegraph._

/**
 * Evaluates `multisched` CodeGraphs.
 */
object Evaluator {
  import Instructions._

  type Env = Map[CodeGraph.NodeKey, Vec4]

  def evaluate(cg: FunctionGraph, inputs: Seq[Any]): (Seq[Any], Map[CodeGraph.NodeKey, Any]) = {
    import CodeGraphOps._

    var env: Map[CodeGraph.NodeKey, Any] = cg.inputs.zip(inputs).toMap
    for (edgeKey <- cg.topSort) {
      val edge = cg.edge(edgeKey).head
      def args = edge.args.map(x => env.getOrElse(x, throw new RuntimeException(s"Node not evaluated: $x")))

      def results = evaluateInstruction(edge.label, args)
      env = env ++ edge.results.zip(results).toMap
    }

    (cg.outputs.map(x => env.getOrElse(x, throw new RuntimeException(s"Node not evaluated: $x"))), env)
  }

  def evaluateInstruction(inst: Instruction, args: Seq[Any]): Seq[Any] = {
    def arg0[T]: T = args(0).asInstanceOf[T]
    def arg1[T]: T = args(1).asInstanceOf[T]
    def arg2[T]: T = args(2).asInstanceOf[T]

    lazy val v0: Vec4 = arg0[Vec4]
    lazy val v1: Vec4 = arg1[Vec4]
    lazy val v2: Vec4 = arg2[Vec4]

    def floatCompare(f: (Float, Float) => Boolean)(x: Float, y: Float): Int =
      if (f(x, y)) { -1 } else { 0 }

    inst match {
      //
      case const(x) =>
        Seq(x)

      //
      case `a` =>
        Seq(v0.zipWith(_ + _)(v1))

      //
      case `and` =>
        Seq(v0.zipWith(_ & _)(v1))

      //
      case `cflts`(exp) =>
        def unflt(x: Float): Int = {
          if (x < 0.5 - exp2f(31)) {
            -exp2(31)
          } else if (x > exp2f(31) - 1.5) {
            exp2(31) - 1
          } else {
            math.floor(x).toInt
          }
        }
        def scale(x: Float): Float = {
          if (exp >= 0 && exp < 128) { x * exp2f(exp) }
          else sys.error("Invalid argument to cflts")
        }
        val Seq(a, b, c, d) =  v0.floatView.floats.map(z => unflt(scale(z)))
        Seq(new Vec4(a, b, c, d))

      //
      case `fcgt` =>
        Seq(v0.floatView.zipWith(floatCompare(_ > _))(v1))

      //
      case `fm` =>
        Seq(v0.floatView.zipWith(_ * _)(v1))

      //
      case `fma` =>
        Seq(v0.floatView.zipWith(_ * _)(v1).floatView.zipWith(_ + _)(v2))

      //
      case `roti`(v) =>
        Seq(v0.map(z => (z << v) | (z >>> (32 - v))))

      //
      case `selb` =>
        Seq(v2.zipWith(_ & _)(v1).zipWith(_ | _)(v2.zipWith(~_ & _)(v0)))

      //
      case `shufb` =>
        val bytes = v0.byteView.bytes ++ v1.byteView.bytes
        def getByte(x: Byte) = x match {
          case b if (b & 0xc0) == 0x80 => 0x00.toByte
          case b if (b & 0xe0) == 0xc0 => 0xff.toByte
          case b if (b & 0xe0) == 0xe0 => 0x80.toByte
          case b => bytes(b % 32)
        }
        Seq(v2.byteView.map(getByte))

      //
      case x => sys.error(s"Invalid instruction evaluated: $x")
    }
  }


}
