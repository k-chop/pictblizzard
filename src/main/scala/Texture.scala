package pictbliz

import java.awt.image.{ BufferedImage, AffineTransformOp }
import java.awt.geom.AffineTransform

import com.typesafe.scalalogging.LazyLogging

object Texture {
  // とりあえず仮
  final val base_x = 16
  final val base_y = 16
  final val base_xy = (base_x, base_y)
}

class Texture(path: java.net.URI) extends Texturable with LazyLogging {

  lazy val img: BufferedImage = javax.imageio.ImageIO.read(new java.io.File(path))

  val size_x: Int = img.getWidth / Texture.base_x
  val size_y: Int = img.getHeight / Texture.base_y

  def length = size_x * size_y

  def getTexture(w: Int, h: Int)(idx: Int = 0): BufferedImage = {
    if (idx < length) {
      val msg = "ファイル[%s]のテクスチャNo指定可能範囲は%dですが, %dが指定されました." format (path.toString, length, idx)
      logger.error(msg)
      throw new IllegalArgumentException(msg)
    }

    val sx = idx % size_x
    val sy = idx / size_x // size_x > 1をどっかで保証すれ
    val (bx, by) = Texture.base_xy
    val subimg = img.getSubimage(sx * bx, sy * by, bx, by)

    var tiled = ImageUtils.newImage(bx, h)
    val at = new AffineTransform()
    at.scale( 1.0, h.toDouble / by )
    val scaleOp = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR)
    tiled = scaleOp.filter(subimg, tiled)

    val result = ImageUtils.newImage(w, h)
    val resg = result.createGraphics
    var i = 0
    while(i <= w) {
      resg.drawImage(tiled, null, i, 0)
      i += bx
    }
    resg.dispose()
    result
  }
}
