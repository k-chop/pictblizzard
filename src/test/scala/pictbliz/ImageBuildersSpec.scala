package pictbliz
package test

import pictbliz.scriptops.Attrs._

class ImageBuildersSpec extends UnitSpec {


  "ImageBuilders" should {
    "find begin point collectly with method findBeginPoint" in {
      val a = Map('point -> APoint(0, 1))
      val b = Map('rect -> ARect(7, 9, 9, 11))
      val c = Map('rect -> ARect(3, 5, 1, 1), 'oncenter -> ANil)
      val d = Map('point -> APoint(160, 120), 'oncenter -> ANil)

      ImageBuilders.findBeginPoint(a, 0, 0) should be (0, 1)
      ImageBuilders.findBeginPoint(b, 0, 0) should be (7, 9)
      ImageBuilders.findBeginPoint(c, 1, 1) should be (3, 5)
      ImageBuilders.findBeginPoint(d, 320, 240) should be (0, 0)
    }
  }

}
