package com.github.whelmaze.pictbliz.test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.whelmaze.pictbliz.{DrawableImage, Drawer}
import com.github.whelmaze.pictbliz.ScriptOps._

class DrawerSpec extends WordSpec with ShouldMatchers {

  "DrawableImage" should {
      "find begin point collectly with method findBeginPoint" in {
      //val drawer = new Drawer(LayoutUnit.empty())
      val dimg = new DrawableImage(null)
      val a = Map('point -> APoint(0, 1))
      val b = Map('rect -> ARect(7, 9, 9, 11))

      dimg.findBeginPoint(a) should be (0, 1)
      dimg.findBeginPoint(b) should be (7, 9)

    }

  }

}
