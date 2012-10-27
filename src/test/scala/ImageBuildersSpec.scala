package com.github.whelmaze.pictbliz.test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.whelmaze.pictbliz.{ImageBuilders, DrawableImage, Drawer}
import com.github.whelmaze.pictbliz.scriptops.Attrs._

class ImageBuildersSpec extends WordSpec with ShouldMatchers {

  "DrawableImage" should {
    "find begin point collectly with method findBeginPoint" in {
      val a = Map('point -> APoint(0, 1))
      val b = Map('rect -> ARect(7, 9, 9, 11))

      ImageBuilders.findBeginPoint(a) should be (0, 1)
      ImageBuilders.findBeginPoint(b) should be (7, 9)

    }

  }

}
