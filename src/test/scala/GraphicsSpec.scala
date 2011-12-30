package com.github.chuwb.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

import java.awt.image.{ BufferedImage }
import java.awt.{ Graphics2D, RenderingHints, Color, Font }
import java.awt.geom.{ Point2D }

import ScriptOps._

class GraphicsSpec extends WordSpec with ShouldMatchers {

  "Graphics2D" should {

    "text antialiasing off" in {
      import RenderingHints._
      
      val img = new BufferedImage(320, 240, BufferedImage.TYPE_INT_ARGB)
      val g2d = img.createGraphics
      g2d.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_GASP)
      g2d.setColor(Color.black)
      // g2d.setFont(Drawer.defaultFont)
      // g2d.drawString("don't panic!", 10, 20)
      val str = "あいうえおsdobuaer1235"
      for (i <- 0 to 6) {
        val wi = 160
        val f = new Font(Font.MONOSPACED, Font.PLAIN, 8+i)
        g2d.setFont(f)
        val frc = g2d.getFontRenderContext
        val v = f.createGlyphVector(frc, str)
        //g2d.drawString("don't panic!", 10, 20+i*30)
        val max = str.length
//        v.setGlyphPosition(max-1, new Point2D.Double(300,20+i*30))
        for(j <- 0 until max) {
//          val rec = v.getGlyphPixelBounds(j, null, 0, 0+i*30)
          val rec = v.getGlyphLogicalBounds(j).getBounds
          val pos = v.getGlyphPosition(j)
          g2d.drawRect( (rec.getX).toInt,
                       (rec.getY + i*30).toInt, rec.getWidth.toInt, rec.getHeight.toInt )
        }
        g2d.drawGlyphVector(v, 0, 0+i*30)
      }
      
      PNGIO.write(img, "temp/GraphicsSpec.png")

      assert(true)
    }
    
  }
  
}
