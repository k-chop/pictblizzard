package com.github.whelmaze.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.image.{ BufferedImage }

object SingleColors {

  lazy val default = {
    new SingleColors(Color.white, Color.red, Color.green, Color.blue, Color.black)
  }

}

class SingleColors(val color: Array[Color]) extends Texturable {
  def length = color.length

  def this(_color: Color*) = this(_color.toArray)
  def this(colornames: Array[String]) = {
    this( colornames map { s => UColor.code(s).self } )
  }

  def getTexture(w: Int, h: Int)(idx: Int) = {
    if (length <= idx) throw new IndexOutOfBoundsException

    val img = ImageUtils.newImage(w, h)
    val g = img.createGraphics

    g.setColor(color(idx))
    g.fillRect(0, 0, w, h)
    img
  }
  
}
