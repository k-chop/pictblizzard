package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.image.{ BufferedImage, AffineTransformOp }
import java.awt.geom.{ AffineTransform }

object SystemGraphics {

  def fromPath(path: java.net.URI): SystemGraphics = {
    new SystemGraphics(path)
  }

  def default: SystemGraphics = {
    sys.error("undefined")
  }
}

class SystemGraphics (path: java.net.URI) extends Texturable {

  val img: BufferedImage = {
    val res = javax.imageio.ImageIO.read(new java.io.File(path))
    if (res.getWidth != 160 || res.getHeight != 80) // もっと適切な例外がある気がする
      throw new IllegalArgumentException("システムグラフィックの画像サイズは160 x 80でなければなりません．")
    else
      res
  }
  
  private val size_x: Int = 10
  private val size_y: Int = 2
  private val unit_w: Int = 16
  private val unit_h: Int = 16
  private val offset_y: Int = 48
  
  def getTexture(w: Int, h: Int)(idx: Int = 0): BufferedImage = {

    if (idx < -1 || 19 < idx)
      throw new IllegalArgumentException("システムグラフィックのカラーインデックスの有効範囲は[-1]から[19]までです．")
    
    val sx = idx % size_x
    val sy = idx / size_x
    
    val subimg = {
      if (idx == -1) // 影色を指定
        img.getSubimage(16, 32, unit_w, unit_h)
      else
        img.getSubimage(sx * unit_w, sy * unit_h + offset_y, unit_w, unit_h)
    }

    val tiled = if (unit_h == h) {
      subimg
    } else {
      var dst = new BufferedImage(unit_w, h, BufferedImage.TYPE_INT_ARGB)
      val at = new AffineTransform()
      at.scale( 1.0, h.toDouble / unit_h )
      val scaleOp = new AffineTransformOp(at, null /* AffineTransformOp.TYPE_BILINEAR */);
      dst = scaleOp.filter(subimg, dst);
      dst
    }

    val result = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val resg = result.createGraphics
    var i = 0
    while(i <= w) {
      resg.drawImage(tiled, null, i, 0)
      i += unit_w
    }
    resg.dispose
    result
  }
  
}
