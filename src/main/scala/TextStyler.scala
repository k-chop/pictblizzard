package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.{ GlyphVector }
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

object TextStyler {
  val default_system_graphics: Texturable = SingleColors.default
}

class TextStyler(val origimg: BufferedImage,
                  val glyphvec: WrappedGlyphVector,
                  val attrmap: AttrMap,
                  val attrstr: AttributedText)
{
  var colors: Texturable = TextStyler.default_system_graphics
  
  def process(): BufferedImage = {
    //println( attrstr.string.split("\n").toList )
    
    //test
    colors = new Texture(Resource.uri("textures_fromrtp.png"))
    colored()

    if (attrmap.contains('border)) bordered(Color.white)
    origimg
  }
  
  // 影つける
  def shadowed(c: Color) = {
    
  }
  // 色つける
  def colored() = {
    import java.awt.image.DataBufferInt
    
    val maskimg = new BufferedImage(origimg.getWidth, origimg.getHeight, BufferedImage.TYPE_INT_ARGB)
    val g = maskimg.createGraphics
    
    for (AttributeRange(begin, end, ctr) <- attrstr.iter) {
      val texIdx = ctr match {
        case CtrColor(idx) => idx
        case CtrNop => 0
      }
      
      // 改行文字みつけたらどうにかするのはこのへん
      val Extractors.Rect2DALL(px, py, pw, ph) = glyphvec.getFixedLogicalBounds(begin, end)

      val paintTex = colors.getTexture(pw, ph)(texIdx)
      (new DrawableImage(paintTex)).write(Resource.tempdir + begin + "_" + end + "c.png")
      g.drawImage(paintTex, null, px, py + glyphvec.ascent.toInt)
      
    }
    
    // 元画像と合成
    val destPixels = (origimg.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData
    val srcPixels  = (maskimg.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData

    for{ // 最適化はあとでやればよし
      idy <- 0 until origimg.getHeight;
      dy = idy * origimg.getWidth;
      idx <- 0 until origimg.getWidth;
      i = dy + idx
      if destPixels(i) != 0x00000000
    } {
      destPixels(i) = srcPixels(i)
    }

  }
  // シスグラの影つける
  // シスグラの色つける
  // ふちどりする

  // border
  // このクラスに置いといたらrectの範囲外に描画することができない…
  // やっぱPaddingは必要なような気がしてきた
  def bordered(c: Color) = {
    val g2d = origimg.createGraphics
    g2d.setPaint(c)
    g2d.drawRect(0, 0, origimg.getWidth-1, origimg.getHeight-1)
  }

}
