package pictbliz

import java.awt.Color

object SingleColors {

  lazy val default = {
    new SingleColors(Color.white, Color.red, Color.green, Color.blue, Color.black, Color.gray)
  }

}

class SingleColors(val color: Array[Color]) extends Texturable {
  def length = color.length

  def this(color: Color*) = this(color.toArray)
  def this(colorNames: Array[String]) = this( colorNames.map { UColor.code(_).self } )

  def getTexture(w: Int, h: Int, idx: Int) = {
    require(idx < length)

    val img = ImageUtils.newImage(w, h)
    val g = img.createGraphics

    g.setColor(color(idx))
    g.fillRect(0, 0, w, h)
    img
  }

  // Shadow color is last element of colors array.
  def getShadowTexture(w: Int, h: Int) = getTexture(w, h, length - 1)
  
}
