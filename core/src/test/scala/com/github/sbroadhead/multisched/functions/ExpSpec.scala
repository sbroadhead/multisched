package com.github.sbroadhead.multisched.functions

import org.specs2.mutable._
import com.github.sbroadhead.codegraph._
import com.github.sbroadhead.multisched._
import com.github.sbroadhead.multisched.functions._

class ExpSpec extends Specification {
  def evalExp(a: Float, b: Float, c: Float, d: Float): (Float, Float, Float, Float) = {
    val results = Evaluator.evaluate(Exp.exp, Seq(new Vec4(a, b, c, d)))._1
    results.head.asInstanceOf[Vec4].floatView.tuple
  }

  "exp codegraph" should {
    "execute correctly" in {
      val (a, b, c, d) = evalExp(1, 2, 3, 4)
      val (fa, fb, fc, fd) = (math.exp(1).toFloat, math.exp(2).toFloat, math.exp(3).toFloat, math.exp(4).toFloat)

      a must beCloseTo(fa, 0.0001f)
      b must beCloseTo(fb, 0.0001f)
      c must beCloseTo(fc, 0.0001f)
      d must beCloseTo(fd, 0.0001f)
    }
  }

}

