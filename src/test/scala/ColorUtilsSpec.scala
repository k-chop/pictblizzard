package com.github.chuwb.pictbliz.test

import com.github.chuwb.pictbliz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class ColorUtilsSpec extends WordSpec with ShouldMatchers {

  "UColor object " should {
    "valid color from color code" in {
      val a = UColor.code("#000000")
      assert(a.r == 0)
      assert(a.g == 0)
      assert(a.b == 0)
      assert(a.a == 0)
    }
  }

  "UColor" should {
    "convert RGB/HSV correctly" in {
      val a = UColor.rgb(105,133,232)
      assert(a.h === 226)
      assert(a.s === 139)
      assert(a.v === 232)
    }

    "has same value when convert HSV->RGB->HSV" in {
      val a = UColor.hsv(30, 200, 202)
      assert(a.r === 202)
      assert(a.g === 123)
      assert(a.b === 44)
      assert(a.h === 30)
      assert(a.s === 199) // 200 -> ? ->199
      assert(a.v === 202)
    }
  }
}
