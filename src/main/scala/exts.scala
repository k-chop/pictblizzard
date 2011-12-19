package com.github.chuwb.pictbliz


object Extractors {

  import java.awt.geom.Rectangle2D
  
  object Rect2D {
    def unapply(rect2d: Rectangle2D): Option[(Int, Int)] = {
      val w = rect2d.getWidth.toInt
      val h = rect2d.getHeight.toInt
      Some(w, h)
    }
  }
  
}
