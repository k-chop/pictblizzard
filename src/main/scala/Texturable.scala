package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.image.{ BufferedImage }

trait Texturable {

  def getTexture(w: Int, h: Int)(idx: Int): BufferedImage
}

case object NullTexture extends Texturable {
  
  def getTexture(w: Int, h: Int)(idx: Int = 0) = {
    new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  }
}
