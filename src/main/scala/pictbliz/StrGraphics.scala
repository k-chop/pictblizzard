package pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.GlyphVector
import java.awt.image.BufferedImage

import com.typesafe.scalalogging.LazyLogging

object StrGraphics extends LazyLogging {

  val DEFAULT_FONT = new Font("MS Gothic", Font.PLAIN, 12)

  def initGraphics2D(g: Graphics2D, font: Font) {
    import java.awt.RenderingHints._
    // VALUE_TEXT_ANTIALIAS_GASP = If bitmap exists, use them.
    // VALUE_TEXT_ANTIALIAS_ON   = always enable antialias.
    // VALUE_TEXT_ANTIALIAS_OFF  = always disable antialias.
    g.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_OFF)
    g.setColor(Color.white)
    g.setFont(font)
  }

  def build(str: String, params: Params): StrGraphics = {
    def extractStyle(s: Symbol): Int = s match {
      case 'plain => Font.PLAIN
      case 'bold => Font.BOLD
      case 'italic => Font.ITALIC
      case 'bolditalic => Font.BOLD | Font.ITALIC
      case n => logger.warn("Undefined font style detected: "+n+", Use default style."); Font.PLAIN
    }

    val font = params.font map { font =>
      val Params.Font(name, style, pt) = font
      new Font(name, extractStyle(style), pt)
    } getOrElse {
      DEFAULT_FONT
    }

    new StrGraphics(str, font, params)
  }


}

class StrGraphics(_str: String,
                  val font: Font,
                  val params: Params)
{
  val g2d = ImageUtils.newImage(1, 1).createGraphics
  StrGraphics.initGraphics2D(g2d, font)

  val strAttrib = new AttributedText(_str)
  val str = strAttrib.str

  def getNewlineCode: Int = {
    val frc = g2d.getFontRenderContext
    val v = font.createGlyphVector(frc, "\n")
    v.getGlyphCode(0)
  }

  def getWrappedGlyphVector: WrappedGlyphVector = {
    StrGraphics.initGraphics2D(g2d, font)
    val frc = g2d.getFontRenderContext
    val lm = font.getLineMetrics(str, frc)
    new WrappedGlyphVector(generateGlyphVector(), params, getNewlineCode, lm.getAscent)
  }

  def processImage(): BufferedImage = {
    val v = getWrappedGlyphVector
    val processedVector = v.process()
    val bufimage = generateImage( processedVector )
    val styler = new TextStyler(bufimage, processedVector, params, strAttrib)
    styler.process()
  }

  def dispose() {
    g2d.dispose()
  }

  /** 
  * Return BufferedImage that output of GlyphVector.
  */ 
  def generateImage(v: WrappedGlyphVector): BufferedImage = {

    def computeSize(v: WrappedGlyphVector) = {
      import scala.math.max
      import util.Rect2DConversion

      val hasRect = params.rect.isDefined

      val (w1, h1) = params.rect.fold((0, 0)) { r =>
        (r.w, r.h)
      }

      val (x, y, w2, h2) = v.getFixedWholeLogicalBounds.xywh
      
      if (params.autoExpand || !hasRect) { // Image size will match larger one.
        val (dx, dy) = params.padding.map { p =>
          (p.x*2, p.y*2)
        } getOrElse ((x*2, y + v.ascent.toInt))
        ( max(w1, w2 + dx - 1), max(h1, h2 + dy - 1) )
        
      } else if (hasRect)
        (w1, h1)
      else sys.error("Huh?")
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
