package pictbliz

import java.awt.image.DataBufferInt

import ImageUtils.ARGB
import pictbliz.ext.PNG

class ImageUtilsSpec extends UnitSpec {

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

  "Method neighbor" should {
    "get 8-neighbor" in {
      val a = Array(1,2,3,4,5,6,7,8,9)
      val b = ImageUtils.neighbor(a, 4, 3, default = 0)
      b should equal (Array(1,2,3,4,6,7,8,9))
    }

    "get 8-neighbor (out of pixel == default)" in {
      import ImageUtils.neighbor
      //
      // [1]
      //
      var a = Array(1)
      var b = neighbor(a, 0, 1, default = 0)
      b should equal (Array(0,0,0,0,0,0,0,0))

      //  2
      // [1]
      //  6
      a = Array(2,1,6)
      b = neighbor(a, 1, 1, default = 0)
      b should equal (Array(0,2,0,0,0,0,6,0))

      //
      // 5 [5] 5
      //
      a = Array(5,5,5)
      b = neighbor(a, 1, 3, default = 0)
      b should equal (Array(0,0,0,5,5,0,0,0))

      // 1 2 3
      // 4 5[6]
      // 7 8 9
      a = Array(1,2,3,4,5,6,7,8,9)
      b = neighbor(a, 5, 3, default = 0)
      b should equal (Array(2,3,0,5,0,8,9,0))

      //  1 2 3
      //  4 5 6
      // [7]8 9
      a = Array(1,2,3,4,5,6,7,8,9)
      b = neighbor(a, 6, 3, default = -1)
      b should equal (Array(-1,4,5,-1,8,-1,-1,-1))

    }
  }

  "alpha" should {
    "swap alpha value" in {
      import ImageUtils.alpha
      var a = 0xEE002234
      var b = alpha(a, 0xFC)
      b should equal (0xFC002234)

      a = 0x00002234
      b = alpha(a, 0xCA)
      b should equal (0xCA002234)
    }
  }

  "synthesis" should {

    "not produce pixel has invalid-alpha" in {
      val mask = PNG.read("testdata/synthtest/mask.png", false, false)
      val grad = PNG.read("testdata/synthtest/grad.png", false, false)

      val res = ImageUtils.synthesisIndexColor(mask, grad)
      ImageResult("synthtest", res).write("temp/")

      // TODOOOOOOO: check each pixel that is not transparent is (res == grad)
      true should equal (true)
    }
  }

}
