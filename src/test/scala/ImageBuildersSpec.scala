package com.github.whelmaze.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.whelmaze.pictbliz.{ImageBuilders, DrawableImage, Drawer}
import com.github.whelmaze.pictbliz.scriptops.Attrs._
import com.github.whelmaze.pictbliz.scriptops.AttrMap

class ImageBuildersSpec extends WordSpec with ShouldMatchers {
  import scriptops.implicits.string2URI

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
