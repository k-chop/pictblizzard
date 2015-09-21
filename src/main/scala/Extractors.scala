package com.github.whelmaze.pictbliz

object Extractors {

  import java.awt.geom.Rectangle2D
  
  object Rect2D {
    def unapply(rect2d: Rectangle2D): Option[(Int, Int)] = {
      val w = rect2d.getWidth.toInt
      val h = rect2d.getHeight.toInt
      Some((w, h))
    }
  }

  object Rect2DALL {
    def unapply(rect2d: Rectangle2D): Option[(Int, Int, Int, Int)] = {
      val w = rect2d.getWidth.toInt
      val h = rect2d.getHeight.toInt
      val x = rect2d.getX.toInt
      val y = rect2d.getY.toInt
      Some((x, y, w, h))
    }
  }
  
  
}
