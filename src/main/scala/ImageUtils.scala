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
    if (src.getType != BufferedImage.TYPE_BYTE_INDEXED)
      throw new IllegalArgumentException()
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

    if (src.getType != BufferedImage.TYPE_INT_ARGB || target.getType != BufferedImage.TYPE_INT_ARGB)
      throw new IllegalArgumentException("source & target's image type should be 'TYPE_INT_ARGB'")
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

  object PNG {
    private[this] val DWORD = new Array[Byte](4)
    
    private[this] val QWORD = new Array[Byte](8)

    private[this] def byte(arr: IndexedSeq[Int]) = arr.map{ _.toByte }
    
    private[this] def unsign(b: Byte): Int = b & 0xFF
    
    private[this] val PNG_IDENTIFER: Array[Byte] =
      byte( Array(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a) ).toArray

    private[this] def DWORD2Long(arr: Array[Byte]): Long = {
      if(arr.length != 4)
        throw new IllegalArgumentException("DWORD2Long accept only Array length 4.")
      val b = arr.map{ i => if (i < 0) (i+256).toLong else i.toLong }
      0L | b(0)<<24 | b(1)<<16 | b(2)<<8 | b(3)
    }
    
    def transparentColor(ref: String): Int = {
      val cnl = new FileInputStream(ref).getChannel
      try {
        val map = cnl.map(FileChannel.MapMode.READ_ONLY, 0, cnl.size())
        map.order(java.nio.ByteOrder.LITTLE_ENDIAN)
        map.get(QWORD)
        if (!(QWORD sameElements PNG_IDENTIFER)) {
          throw new IllegalArgumentException("ヘッダが不正です.")
        }
        var result: Int = 0x0
        while(map.hasRemaining) {
          map.get(DWORD)
          val len = DWORD2Long(DWORD)
          map.get(DWORD)
          val name = DWORD map { _.toChar } mkString ""
          println("name:"+name)
          if (name == "PLTE") {
            val r = map.get
            val g = map.get
            val b = map.get
            result = unsign(b) | (unsign(g) << 8) | (unsign(r) << 16) | 0xff000000 & 0xffffffff
            map.position(map.position - 3 + len.toInt + 4)
          } else {
            println("lim:"+map.limit)
            println("pos:"+map.position)
            println("next:"+(map.position + len.toInt))
            map.position(map.position + len.toInt + 4)
          }
        }
        result
      } finally {
        cnl.close()
      }
       
    } 
  
  }
  
  object BMP {
    
    
  }
}
