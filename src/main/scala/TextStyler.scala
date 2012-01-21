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

  def process(): BufferedImage = {
    //println( attrstr.string.split("\n").toList )
    if (attrmap.contains('border)) bordered(Color.white)
    origimg
  }
  
  // 影つける
  def shadowed(c: Color) = {
    
  }
  // 色つける
  def colored() = {
    
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
