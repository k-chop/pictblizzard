package pictbliz

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

  val img: RawIndexColorImage = {
    import enrich.bufferedimage._

    val res = ext.PNG.read(path).toRaw
    
    if (res.width != 160 || res.height != 80)
      throw new IllegalArgumentException("SystemGraphic's image size must be 160 x 80.")
    else
      res
  }

  private val size_x: Int = 10
  private val size_y: Int = 2
  private val unit_w: Int = 16
  private val unit_h: Int = 16
  private val offset_y: Int = 48
  
  def getTexture(w: Int, h: Int, idx: Int = 0): RawIndexColorImage = {

    if (idx < 0 || 20 < idx)
      throw new IllegalArgumentException("システムグラフィックのカラーインデックスの有効範囲は[0]から[20(影色)]までです．")
    
    val sx = idx % size_x
    val sy = idx / size_x
    
    val subimg = {
      if (idx == 20) // 影色を指定
        img.trimmed(16, 32, unit_w, unit_h)
      else
        img.trimmed(sx * unit_w, sy * unit_h + offset_y, unit_w, unit_h)
    }

    val tiled = if (unit_h == h) {
      subimg
    } else {
      // TODO: BILINEAR!!
      subimg.fitted(unit_w, h)
    }

    val result = ImageUtils.newRawImage(w, h)
    var i = 0
    while(i <= w) {
      result.drawImage(tiled, i, 0)
      i += unit_w
    }
    result
  }

  def getShadowTexture(w: Int, h: Int): RawIndexColorImage = getTexture(w, h, 20)

  def getSystemWindow(_w: Int, _h: Int, zoom: Boolean = false): RawIndexColorImage = {
    // so many magic numbers lol

    val w = math.max(_w, 16)
    val h = math.max(_h, 16)
    
    val dest = ImageUtils.newRawImage(w, h)

    val bg = img.trimmed(0, 0, 32, 32)
    if (zoom) {
      dest.drawImage(bg.fitted(w, h), 0, 0)
    } else {
      @scala.annotation.tailrec
      def drawTile(x: Int, y: Int) {
        if (x >= w) {
          drawTile(0, y + 32)
        } else if (y >= h) {
          //return
        } else {
          dest.drawImage(bg, x, y)
          drawTile(x + 32, y)
        }
      }
      drawTile(0, 0)
    }

    val ltp = img.trimmed(32, 0, 8, 8)
    val rtp = img.trimmed(56, 0, 8, 8)
    val lbp = img.trimmed(32,24, 8, 8)
    val rbp = img.trimmed(56,24, 8, 8)
    val tp =  img.trimmed(40, 0,16, 8)
    val rp =  img.trimmed(56, 8, 8,16)
    val lp =  img.trimmed(32, 8, 8,16)
    val bp =  img.trimmed(40,24,16, 8)

    dest.drawImage(ltp, 0, 0)
    dest.drawImage(rtp, w-8, 0)
    dest.drawImage(lbp, 0, h-8)
    dest.drawImage(rbp, w-8, h-8)
    
    var restX = w - 16
    var drawX = 8
    while(restX >= 16) {
      dest.drawImage(tp, drawX, 0)
      dest.drawImage(bp, drawX, h-8)
      restX -= 16
      drawX += 16
    }
    if (restX > 0) {
      dest.drawImage(tp.trimmed(0, 0, restX, 8), drawX, 0)
      dest.drawImage(bp.trimmed(0, 0, restX, 8), drawX, h-8)
    }

    var restY = h - 16
    var drawY = 8
    while(restY >= 16) {
      dest.drawImage(lp, 0, drawY)
      dest.drawImage(rp, w-8, drawY)
      restY -= 16
      drawY += 16
    }
    if (restY > 0) {
      dest.drawImage(lp.trimmed(0, 0, 8, restY), 0, drawY)
      dest.drawImage(rp.trimmed(0, 0, 8, restY), w-8, drawY)
    }

    if(_w < 16 || _h < 16) {
      dest.trimmed(0, 0, _w, _h)
    } else dest
  }
  
}
