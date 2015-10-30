package pictbliz

trait Texturable {

  def length: Int
  def getTexture(w: Int, h: Int, idx: Int): RawIndexColorImage
  def getShadowTexture(w: Int, h: Int): RawIndexColorImage
}

case object NullTexture extends Texturable {

  def length = 0
  def getTexture(w: Int, h: Int, idx: Int = 0): RawIndexColorImage = {
    ImageUtils.newRawImage(w, h)
  }
  def getShadowTexture(w: Int, h: Int): RawIndexColorImage = getTexture(w, h)
}
