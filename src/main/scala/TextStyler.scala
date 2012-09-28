package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.{ GlyphVector }
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

class TextStyler(val origimg: BufferedImage,
                  val glyphvec: WrappedGlyphVector,
                  val attrmap: AttrMap,
                  val attrstr: AttributedText)
{
  var colors: Texturable = {
    AttrMap.findParam(attrmap, 'front_color) match {
      case Some(ASystemGraphics(path: String)) =>
        SystemGraphics.fromPath(path)
      case Some(ASingleColors(xs: Array[String])) =>
        new SingleColors(xs)
      case None =>
        SystemGraphics.default
    }
  }
  var test = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int)];
  val debug = attrmap.contains('debug)
  
  def process(): BufferedImage = {
    val dest = ImageUtils.sameSizeImage(origimg)
    val s = shadowed()
    val c = colored()

    val g = dest.createGraphics
    g.drawImage(s, null, 1, 1) // shadow offset = 1
    g.drawImage(c, null, 0, 0)
    g.dispose()
    
    if (attrmap.contains('border)) bordered(Color.white)

    if (debug) {
      val b = dest.createGraphics()
      b.setPaint(Color.black)
      test foreach {
        case (x, y, w, h) =>
          b.drawRect(x, y + glyphvec.ascent.toInt, w, h)
      }
      b.dispose()
    }
    
    dest
  }
  
  // 影つける
  def shadowed(): BufferedImage = colors match {
    case s: SystemGraphics => {
      val maskimg = ImageUtils.copy(origimg)
      val targetimg = ImageUtils.newImage(maskimg.getWidth, maskimg.getHeight)
      val g = targetimg.createGraphics
      val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedWholeLogicalBounds
      val paintTex = colors.getTexture(pw, ph)(20)
      g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)
      g.dispose()
      
      ImageUtils.synthesis(maskimg, targetimg)
    }
    case _ => sys.error("not implemented")
  }
  // 色つける
  def colored(): BufferedImage = {
    val maskimg = ImageUtils.copy(origimg)
    val targetimg = ImageUtils.newImage(maskimg.getWidth, maskimg.getHeight)
    val g = targetimg.createGraphics
  
    for (AttributeRange(begin, end, ctr) <- attrstr.iter) {
      val texIdx = ctr match {
        case CtrColor(idx) => idx
        case CtrNop => 0
      }

      @scala.annotation.tailrec
      def drawEachLine(b: Int, l: List[String]): Unit = l match {
        case Nil => return
        case head :: rest =>
          val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedLogicalBounds(b, b + head.length)
          if (debug) test += ((px, py, pw, ph))
          val paintTex = colors.getTexture(pw, ph)(texIdx)
          g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)

          drawEachLine(b + head.length + 1, rest)
      }
      val subs = attrstr.str.substring(begin, end)
      drawEachLine(begin, subs.split("\n").toList)
    }    
    g.dispose()
    
    ImageUtils.synthesis(maskimg, targetimg)
    maskimg
  }
  // ふちどりする
  // def hem() = {}
  
  def bordered(c: Color) = {
    val g2d = origimg.createGraphics
    g2d.setPaint(c)
    g2d.drawRect(0, 0, origimg.getWidth-1, origimg.getHeight-1)
  }

  
}
