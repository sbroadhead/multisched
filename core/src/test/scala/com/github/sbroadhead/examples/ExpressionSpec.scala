package com.github.sbroadhead.examples

import org.specs2.mutable._
import shapeless._

class ExpressionSpec extends Specification {
  import Expressions._

  "simpleExprCodeGraph" should {
    "evaluate correctly" in {
      evaluate(simpleExprCodeGraph, Seq(10, 20)) must_== Seq(170, true)
      evaluate(simpleExprCodeGraph, Seq(1, 2)) must_== Seq(-1, false)
    }
  }

  "PolynomialCodeGraph" should {
    val cg3 = PolynomialGraph(Seq(1, 2, 3))
    val cg5 = PolynomialGraph(Seq(2, 4, 6, 8, 10))

    "evaluate correctly" in {
      evaluate(cg3, Seq(10)) must_== Seq(3 + 2*10 + 1*100)
      evaluate(cg5, Seq(10)) must_== Seq(10 + 8*10 + 6*100 + 4*1000 + 2*10000)
    }
  }

  "spliceCodeGraph" should {
    "evaluate correctly" in {
      evaluate(spliceCodeGraph, Seq(10, 20)) must_== Seq(8 + 6*170 + 4*(170*170) + 2*(170*170*170))
    }
  }

}
