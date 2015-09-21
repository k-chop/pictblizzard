package pictbliz

import java.awt.image.BufferedImage

trait Texturable {
  def length: Int
  def getTexture(w: Int, h: Int)(idx: Int): BufferedImage
}

case object NullTexture extends Texturable {

  def length = 0
  def getTexture(w: Int, h: Int)(idx: Int = 0) = {
    ImageUtils.newImage(w, h)
  }
}
