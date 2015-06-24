package com.github.sbroadhead.examples

import org.specs2.mutable._
import shapeless._

class ExpressionSpec extends Specification {
  import Expressions._

  "simpleExprCodeGraph" should {
    "be structured correctly" in {
      simpleExprCodeGraph.nodes.size must_== 7
      simpleExprCodeGraph.edges.size must_== 5
    }

    "evaluate correctly" in {
      evaluate(simpleExprCodeGraph, Seq(10, 20)) must_== Seq(170, true)
      evaluate(simpleExprCodeGraph, Seq(1, 2)) must_== Seq(-1, false)
    }

  }

}
