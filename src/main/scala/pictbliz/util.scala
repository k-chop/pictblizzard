package pictbliz

import java.awt.geom.Rectangle2D

object util {

  implicit class Rect2DConversion(val underlying: Rectangle2D) extends AnyVal {

    def xy = (underlying.getX.toInt, underlying.getY.toInt)

    def wh = (underlying.getWidth.toInt, underlying.getHeight.toInt)

    def xywh = {
      val p = xy
      val s = wh
      (p._1, p._2, s._1, s._2)
    }

  }
}
