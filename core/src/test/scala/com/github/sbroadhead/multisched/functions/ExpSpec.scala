package com.github.sbroadhead.multisched.functions

import org.specs2.mutable._
import shapeless._

import com.github.sbroadhead._
import codegraph._

class ExpSpec extends Specification {

  "exp codegraph" should {
    "be built correctly" in {
      println(Exp.codeGraph)
      1 must_== 1
    }
  }

}

