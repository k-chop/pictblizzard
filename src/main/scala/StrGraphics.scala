package com.github.whelmaze.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.GlyphVector
import java.awt.image.BufferedImage

import scriptops.Attrs._

object StrGraphics {
  
    def initGraphics2D(g: Graphics2D, font: Font) {
    import java.awt.RenderingHints._
    // アンチエイリアス。設定によって変更可能にするかも。その場合AttrMapを読んで変える。
    // VALUE_TEXT_ANTIALIAS_GASP = ビットマップがあるならアンチなしでそれ使う。
    // VALUE_TEXT_ANTIALIAS_ON   = 常にアンチエイリアスかける
    // VALUE_TEXT_ANTIALIAS_OFF  = 常にアンチエイリアス無効
    g.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_OFF)
    g.setColor(Color.white)
    g.setFont(font)
  }
  
}

/** 
* StrGraphics
* 
* 渡されるGraphics2DはFontRenderContext構築のためのものなので描画はしない
*/
class StrGraphics(val g2d: Graphics2D,
                  _str: String,
                  val font: Font,
                  val attrmap: AttrMap)
{

  val strAttrib = new AttributedText(_str)
  val str = strAttrib.str
  
  def getNewlineCode: Int = {
    val frc = g2d.getFontRenderContext
    val v = font.createGlyphVector(frc, "\n")
    v.getGlyphCode(0)
  }
  
  def processImage(): BufferedImage = {
    StrGraphics.initGraphics2D(g2d, font)

    val frc = g2d.getFontRenderContext
    val lm = font.getLineMetrics(str, frc)

    val v = new WrappedGlyphVector( generateGlyphVector(), attrmap, getNewlineCode, lm.getAscent )
    val processedVector = v.process()
    val bufimage = generateImage( processedVector )
    val styler = new TextStyler(bufimage, processedVector, attrmap, strAttrib)
    styler.process()
  }

  /** 
  * GlyphVectorの中身をBufferedImageに書き出して返す。
  */ 
  def generateImage(v: WrappedGlyphVector): BufferedImage = {

    def computeSize(v: WrappedGlyphVector) = {
      import scala.math.max

      val hasAutoexp = attrmap.contains('autoexpand)
      val hasRect = attrmap.contains('rect)
      
      val (w1, h1) = if (hasRect) {
        val ARect(_, _, w, h) = attrmap('rect)
        (w, h)
      } else (0, 0)

      val Extractors.Rect2DALL(x, y, w2, h2) = v.getFixedWholeLogicalBounds
      
      if (hasAutoexp || !hasRect) { // どちらか大きい方に拡大される
        val (dx, dy) = attrmap.get('padding) collect {
          case APadding(xp, yp) => (xp*2, yp*2)
        } getOrElse (x*2, y + v.ascent.toInt)
        ( max(w1, w2 + dx - 1), max(h1, h2 + dy - 1) )
        
      } else if (hasRect) // rectの定義そのまま
        (w1, h1)
      else sys.error("えっ")
   	}

    val (w, h) = computeSize(v)

    val buf = ImageUtils.newImage(w, h)
    val g = buf.createGraphics
    StrGraphics.initGraphics2D(g, font)
    g.drawGlyphVector(v.self, 0, v.ascent)
    g.dispose()
    buf
  }
  
  def generateGlyphVector(): GlyphVector = {
    val frc = g2d.getFontRenderContext
    font.createGlyphVector(frc, str)
  }

}
