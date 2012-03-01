package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.font.{ GlyphVector }
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }
import java.nio.ByteBuffer
import java.io.FileInputStream
import java.nio.channels.FileChannel
import java.awt.image.DataBufferInt
import java.awt.image.DataBufferByte

object ImageUtils {

  def newImage(w: Int, h: Int): BufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  def newImage(size: (Int, Int)): BufferedImage = newImage(size._1, size._2)
  
  def pile(src: BufferedImage, targets: BufferedImage*): BufferedImage = {
    src
  }
  
  def sameSizeImage(src: BufferedImage): BufferedImage = 
    new BufferedImage(src.getWidth, src.getHeight, src.getType)
  
  def copy(src: BufferedImage): BufferedImage = {
    val dest = sameSizeImage(src)
    dest.setData(src.getData)
    dest
  }
  
  def indexedColorToARGBImage(src: BufferedImage): BufferedImage = {
    require(src.getType == BufferedImage.TYPE_BYTE_INDEXED, src.getType())
    
    val dest = newImage(src.getWidth, src.getHeight)
    val srcPixel = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferByte].getData
    val destPixel = (dest.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData
    val cm = src.getColorModel()
    for (i <- 0 until srcPixel.length) {
      destPixel(i) = (0xff000000|0x00ffffff&(cm.getRGB(srcPixel(i)))) 
    }
    dest
  }
  
  def enableAlpha(src: BufferedImage, transColor: Int): BufferedImage = {
    val srcPixel = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData

    for (i <- 0 until srcPixel.length if srcPixel(i) == transColor)
      srcPixel(i) = 0x0
    src
  }
  
  def synthesis(src: BufferedImage, target: BufferedImage, maskcolor: Int = 0xFFFFFFFF): BufferedImage = {
    require(src.getType == BufferedImage.TYPE_INT_ARGB && target.getType == BufferedImage.TYPE_INT_ARGB,
        "source & target's image type should be 'TYPE_INT_ARGB'")

    val srcPixel    = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData
    val targetPixel = (target.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData

    for {
      idy <- 0 until src.getHeight;
      dy = idy * src.getWidth;
      idx <- 0 until src.getWidth;
      i = dy + idx
      if srcPixel(i) == maskcolor
    } {
      srcPixel(i) = targetPixel(i)
    }
    
    src
  }
}
