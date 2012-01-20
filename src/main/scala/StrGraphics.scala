package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.{ GlyphVector }
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

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
  val str = strAttrib.string
  
  def getNewlineCode(): Int = {
    val frc = g2d.getFontRenderContext()
    val v = font.createGlyphVector(frc, "\n")
    v.getGlyphCode(0)
  }
  
  def processImage(): BufferedImage = {
    StrGraphics.initGraphics2D(g2d, font)
    val v = new WrappedGlyphVector( generateGlyphVector, attrmap, getNewlineCode )
    val processedVector = v.process
    val bufimage = generateImage( processedVector )
    //val styler = new TextStyler(bufimage, processedVector, attrmap, strAttrib)
    //styler.process
    bufimage
  }

  /** 
  * GlyphVectorの中身をBufferedImageに書き出して返す。
  */ 
  def generateImage(v: WrappedGlyphVector): BufferedImage = {

    def computeSize(v: WrappedGlyphVector) = {
      import scala.math.{ max }
      import Extractors.Rect2D

      val hasAutoexp = attrmap.contains('autoexpand)
      val hasRect = attrmap.contains('rect)
      val ARect(_, _, w1, h1) = attrmap('rect)
      val Rect2D(w2, h2) = v.getFixedLogicalBounds

      if (hasAutoexp || !hasRect) // どちらか大きい方に拡大される
        ( max(w1, w2), max(h1, h2) )
      else if (hasRect) // rectの定義そのまま
        (w1, h1)
      else sys.error("えっ")
    }

    val (w, h) = computeSize(v)
    val buf = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val g = buf.createGraphics
    StrGraphics.initGraphics2D(g, font)
    val frc = g.getFontRenderContext
    val lm = font.getLineMetrics(str, frc)
    g.drawGlyphVector(v.self, 0, lm.getAscent)
    buf
  }
  
  def generateGlyphVector(): GlyphVector = {
    val frc = g2d.getFontRenderContext()
    font.createGlyphVector(frc, str)
  }

}
