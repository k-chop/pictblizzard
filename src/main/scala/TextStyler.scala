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
    extractColorMap
    println( attrstr.string.split("\n").toList )
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
}
