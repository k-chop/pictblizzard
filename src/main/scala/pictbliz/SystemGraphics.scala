package pictbliz

import java.awt.image.{ BufferedImage, AffineTransformOp }
import java.awt.geom.AffineTransform
import java.nio.file.Path

import com.typesafe.scalalogging.LazyLogging

object SystemGraphics {
  import ext.FilePath._

  def make[T: ToPath](path: T): SystemGraphics =
    new SystemGraphics(implicitly[ToPath[T]].toPath(path))

  def default: SystemGraphics =
    make("testdata/no-v/systemrtp2000.png")
}

class SystemGraphics (path: Path) extends Texturable with LazyLogging {

  def length = 21

  val img: BufferedImage = {

    val res = ext.PNG.read(path, argb = true)
    
    if (res.getWidth != 160 || res.getHeight != 80)
      throw new IllegalArgumentException("SystemGraphic's image size must be 160 x 80.")
    else
      res
  }
  
  lazy val pltezero: Int = {
    val res = ext.PNG.transparentColor(path)
    logger.trace(
      "%s\nこのファイルの透過色は, ARGBの順に%d,%d,%d,%dです." format (path,res>>24&0xff,res>>16&0xff,res>>8&0xff,res&0xff)
    )
    res
  }
  
  private val size_x: Int = 10
  private val size_y: Int = 2
  private val unit_w: Int = 16
  private val unit_h: Int = 16
  private val offset_y: Int = 48
  
  def getTexture(w: Int, h: Int, idx: Int = 0): BufferedImage = {

    if (idx < 0 || 20 < idx)
      throw new IllegalArgumentException("システムグラフィックのカラーインデックスの有効範囲は[0]から[20(影色)]までです．")
    
    val sx = idx % size_x
    val sy = idx / size_x
    
    val subimg = {
      if (idx == 20) // 影色を指定
        img.getSubimage(16, 32, unit_w, unit_h)
      else
        img.getSubimage(sx * unit_w, sy * unit_h + offset_y, unit_w, unit_h)
    }

    val tiled = if (unit_h == h) {
      subimg
    } else {
      var dst = ImageUtils.newImage(unit_w, h)
      val at = new AffineTransform()
      at.scale( 1.0, h.toDouble / unit_h )
      //val scaleOp = new AffineTransformOp(at, null: java.awt.RenderingHints);
      val scaleOp = new AffineTransformOp(at, AffineTransformOp.TYPE_BILINEAR)
      dst = scaleOp.filter(subimg, dst)
      dst
    }

    val result = ImageUtils.newImage(w, h)
    val resg = result.createGraphics
    var i = 0
    while(i <= w) {
      resg.drawImage(tiled, null, i, 0)
      i += unit_w
    }
    resg.dispose()
    result
  }

  def getShadowTexture(w: Int, h: Int): BufferedImage = getTexture(w, h, 20)

  def getSystemWindow(w: Int, h: Int, zoom: Boolean = false): BufferedImage = {
    // so many magic numbers lol

    require(16 <= w && 16 <= h, s"texture width and height must be 16 or above (w: $w, h: $h)")
    
    var dest = ImageUtils.newImage(w, h)

    val bg = img.getSubimage(0, 0, 32, 32)
    if (zoom) {
      val at = new AffineTransform()
      at.scale(w / 32.0, h / 32.0)
      val scaleOp = new AffineTransformOp(at, null: java.awt.RenderingHints)
      dest = scaleOp.filter(bg, dest)
    } else {
      val g = dest.createGraphics
      
      @scala.annotation.tailrec
      def drawTile(x: Int, y: Int) {
        if (x >= w) {
          drawTile(0, y + 32)
        } else if (y >= h) {
          //return
        } else {
          g.drawImage(bg, null, x, y)
          drawTile(x + 32, y)
        }
      }
      drawTile(0, 0)
      g.dispose()
    }

    ImageUtils.enableAlpha(img, pltezero)
    
    val ltp = img.getSubimage(32, 0, 8, 8)
    val rtp = img.getSubimage(56, 0, 8, 8)
    val lbp = img.getSubimage(32,24, 8, 8)
    val rbp = img.getSubimage(56,24, 8, 8)
    val tp =  img.getSubimage(40, 0,16, 8)
    val rp =  img.getSubimage(56, 8, 8,16)
    val lp =  img.getSubimage(32, 8, 8,16)
    val bp =  img.getSubimage(40,24,16, 8)

    val g = dest.createGraphics

    g.drawImage(ltp, null, 0, 0)
    g.drawImage(rtp, null, w-8, 0)
    g.drawImage(lbp, null, 0, h-8)
    g.drawImage(rbp, null, w-8, h-8)
    
    var restx = w - 16
    var drawx = 8
    while(restx >= 16) {
      g.drawImage(tp, null, drawx, 0)
      g.drawImage(bp, null, drawx, h-8)
      restx -= 16
      drawx += 16
    }
    if (restx > 0) {
      g.drawImage(tp.getSubimage(0, 0, restx, 8), null, drawx, 0)
      g.drawImage(bp.getSubimage(0, 0, restx, 8), null, drawx, h-8)
    }

    var resty = h - 16
    var drawy = 8
    while(resty >= 16) {
      g.drawImage(lp, null, 0, drawy)
      g.drawImage(rp, null, w-8, drawy)
      resty -= 16
      drawy += 16
    }
    if (resty > 0) {
      g.drawImage(lp.getSubimage(0, 0, 8, resty), null, 0, drawy)
      g.drawImage(rp.getSubimage(0, 0, 8, resty), null, w-8, drawy)
    }
    
    g.dispose()
    dest
  }
  
}
