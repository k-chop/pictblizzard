package pictbliz

import java.awt.Color

import enrich.packedcolor._

object SingleColors {

  lazy val default = {
    new SingleColors(Color.white, Color.red, Color.green, Color.blue, Color.black, Color.gray)
  }

}

class SingleColors(val color: Array[Color]) extends Texturable {

  def length = color.length

  def this(color: Color*) = this(color.toArray)
  def this(color: Array[Int]) = this(color.map(_.asColor): Array[Color])

  def getTexture(w: Int, h: Int, idx: Int): RawIndexColorImage = {
    require(idx < length)

    val img = ImageUtils.newRawImage(w, h)
    img.fill(color(idx).getRGB)
    img
  }

  // Shadow color is last element of colors array.
  def getShadowTexture(w: Int, h: Int): RawIndexColorImage = getTexture(w, h, length - 1)
  
}
