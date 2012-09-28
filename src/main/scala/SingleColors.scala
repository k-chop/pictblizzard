package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.image.{ BufferedImage }

object SingleColors {

  lazy val default = {
    new SingleColors(Color.white, Color.red, Color.green, Color.blue, Color.black)
  }

}

class SingleColors(_color: java.awt.Color*) extends Texturable {

  val color = _color.toArray
  def length = color.length

  def getTexture(w: Int, h: Int)(idx: Int) = {
    if (length <= idx) throw new IndexOutOfBoundsException

    val img = ImageUtils.newImage(w, h)
    val g = img.createGraphics

    g.setColor(color(idx))
    g.fillRect(0, 0, w, h)
    img
  }
  
}
