package pictbliz

import com.typesafe.scalalogging.LazyLogging

import enrich.bufferedimage._

object Texture {
  // とりあえず仮
  final val base_x = 16
  final val base_y = 16
  final val base_xy = (base_x, base_y)
}

class Texture(path: java.net.URI) extends Texturable with LazyLogging {

  lazy val img: RawIndexColorImage = ext.PNG.read(new java.io.File(path)).toRaw

  val size_x: Int = math.max(img.width / Texture.base_x, 1)
  val size_y: Int = math.max(img.height / Texture.base_y, 1)

  def length = size_x * size_y

  def getTexture(w: Int, h: Int, idx: Int = 0): RawIndexColorImage = {

    if (idx < length) {
      val msg = s"texture file [${path.toString}] accept only texture-index 0-$length. idx: $idx"
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }

    val sx = idx % size_x
    val sy = idx / size_x
    val (bx, by) = Texture.base_xy
    val subimg = img.trimmed(sx * bx, sy * by, bx, by)

    // TODO: BILINEAR!!
    val tiled = subimg.fitted(bx, h)

    val result = ImageUtils.newRawImage(w, h)
    var i = 0
    while(i <= w) {
      result.drawImage(tiled, i, 0)
      i += bx
    }
    result
  }

  // Shadow texture is last element of textures.
  def getShadowTexture(w: Int, h: Int): RawIndexColorImage = getTexture(w, h, length - 1)
}
