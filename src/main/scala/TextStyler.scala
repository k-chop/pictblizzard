package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.{ GlyphVector }
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

/** 
* TextStyler
* 
*   
*/
class TextStyler(val origimg: BufferedImage,
                  val glyphvec: WrappedGlyphVector,
                  val attrmap: AttrMap,
                  val attranges: AttributedText)
{

  def process(): BufferedImage = {
    sys.error("mada dayo")
  }
  
  // 影つける
  def shadow(c: Color) = {
    
  }
  // 色つける
  // シスグラの影つける
  // シスグラの色つける
  // ふちどりする
}
