package pictbliz

import pictbliz.ext.PNG

class ImageUtilsSpec extends UnitSpec {

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

}
