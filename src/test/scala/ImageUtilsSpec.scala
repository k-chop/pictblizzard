package com.github.whelmaze.pictbliz.test

import com.github.whelmaze.pictbliz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import ImageUtils.ARGB

class ImageUtilsSpec extends WordSpec with ShouldMatchers {

  "ARGB object" should {
    "extract 0xAARRGGBB with unapply" in {
      val p1 = 0xFFAABB88
      val ARGB(a1,r1,g1,b1) = p1
      a1 should equal (0xFF)
      r1 should equal (0xAA)
      g1 should equal (0xBB)
      b1 should equal (0x88)

      val p2 = 0x0123153F
      val ARGB(a2,r2,g2,b2) = p2
      a2 should equal (0x01)
      r2 should equal (0x23)
      g2 should equal (0x15)
      b2 should equal (0x3F)

      val p3 = 0xFFFFFFFF
      val ARGB(a3,r3,g3,b3) = p3
      a3 should equal (0xFF)
      r3 should equal (0xFF)
      g3 should equal (0xFF)
      b3 should equal (0xFF)

      val p4 = 0x0
      val ARGB(a4,r4,g4,b4) = p4
      a4 should equal (0x00)
      r4 should equal (0x00)
      g4 should equal (0x00)
      b4 should equal (0x00)

      val p5 = -2
      val ARGB(a5,r5,g5,b5) = p5
      a5 should equal (0xFF)
      r5 should equal (0xFF)
      g5 should equal (0xFF)
      b5 should equal (0xFE)


    }
  }

}
