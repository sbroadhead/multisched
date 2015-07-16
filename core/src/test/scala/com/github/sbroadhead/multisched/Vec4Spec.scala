package com.github.sbroadhead.multisched

import org.specs2.mutable._

class Vec4Spec extends Specification {

  "Vec4" should {
    val v1 = new Vec4(1.0f, 2.0f, 3.0f, 4.0f)
    val v2 = new Vec4(2.0f, 4.0f, 6.0f, 8.0f)
    val v3 = new Vec4(100, 200, 300, 400)
    val v4 = new Vec4(25, 50, 75, 100)
    val v5 = new Vec4(Seq.fill(16)(16.toByte))
    val v6 = new Vec4(Seq.range(0.toByte, 16.toByte))

    "map floats correctly" in {
      v1.floatView.map(x => x) must_== v1
      v1.floatView.map(_ * 3.3f) must_== new Vec4(3.3f, 6.6f, 9.9f, 13.2f)
    }

    "zip floats correctly" in {
      v1.floatView.zipWith(_ * _)(v2) must_== new Vec4(2.0f, 8.0f, 18.0f, 32.0f)
    }

    "map words correctly" in {
      v1.map(x => x) must_== v1
      v3.map(_ + 5) must_== new Vec4(105, 205, 305, 405)
    }

    "zip words correctly" in {
      v3.zipWith(_ + _)(v4) must_== new Vec4(125, 250, 375, 500)
    }

    "map bytes correctly" in {
      v5.byteView.map(x => x) must_== v5
      v5.byteView.map(x => (x + 10).toByte) must_== new Vec4(Seq.fill[Byte](16)(26))
    }

    "zip bytes correctly" in {
      v5.byteView.zipWith((x, y) => (x + y).toByte)(v6).byteView.bytes must_== new Vec4(Seq.range(16.toByte, 32.toByte)).byteView.bytes
    }

    "construct from bytes correctly" in {
      new Vec4(Seq.fill(16)(0x80.toByte)) must_== new Vec4(0x80808080)
    }
  }

}

