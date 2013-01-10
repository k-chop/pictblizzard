package com.github.whelmaze.pictbliz

import java.awt.image.{BufferedImage, DataBufferInt, DataBufferByte}

object ImageUtils {

  object ARGB {
    def unapply(c: Int): Option[(Int, Int, Int, Int)] = {
      val B = c & 0xFF
      val G = (c >> 8) & 0xFF
      val R = (c >> 16) & 0xFF
      val A = (c >> 24) & 0xFF
      Some((A, R, G, B))
    }
  }

  @inline def neighbor(arr: Array[Int], i: Int)(implicit wh: (Int, Int)): Array[Int] = {
    // nante hidoi code nanda...
    val (w, h) = wh
    val a = Array.ofDim[Int](8)
    val u = i < w
    val b = (arr.length - i) <= w
    val r = (i+1)%w == 0
    val l = i%w == 0
    a(0) = if (u || l) -1 else i-1-w
    a(1) = if (u) -1 else i-w
    a(2) = if (u || r) -1 else i+1-w
    a(3) = if (l) -1 else i-1
    a(4) = if (r) -1 else i+1
    a(5) = if (b || l) -1 else i-1+w
    a(6) = if (b) -1 else i+w
    a(7) = if (b || r) -1 else i+1+w
    a.map { i=>
      if (0 <= i && i < arr.length) arr(i) else 0
    }
  }

  def newImage(w: Int, h: Int): BufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  def newImage(size: (Int, Int)): BufferedImage = newImage(size._1, size._2)
  
  def pile(src: BufferedImage, targets: BufferedImage*): BufferedImage = ???
  
  def sameSizeImage(src: BufferedImage): BufferedImage = 
    new BufferedImage(src.getWidth, src.getHeight, src.getType)
  
  def copy(src: BufferedImage): BufferedImage = {
    val dest = sameSizeImage(src)
    dest.setData(src.getData)
    dest
  }
  
  def toARGBImage(src: BufferedImage): BufferedImage = {
    if (src.getType != BufferedImage.TYPE_BYTE_INDEXED)
      return src
    
    val dest = newImage(src.getWidth, src.getHeight)
    val srcPixel = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferByte].getData
    val destPixel = (dest.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData
    val cm = src.getColorModel
    (0 until srcPixel.length) foreach { i =>
      destPixel(i) = (0xff000000|0x00ffffff&(cm.getRGB(srcPixel(i)))) 
    }
    dest
  }
  
  def enableAlpha(src: BufferedImage, transColor: Int): BufferedImage = {
    val srcPixel = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData

    (0 until srcPixel.length) foreach { i =>
      if (srcPixel(i) == transColor)
        srcPixel(i) = 0x0
    }
    src
  }
  
  def synthesis(src: BufferedImage, target: BufferedImage, maskcolor: Int = 0xFFFFFFFF): BufferedImage = {
    require(src.getType == BufferedImage.TYPE_INT_ARGB && target.getType == BufferedImage.TYPE_INT_ARGB,
        "source & target's image type should be 'TYPE_INT_ARGB'")

    val srcPixel    = (src.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData
    val targetPixel = (target.getRaster.getDataBuffer).asInstanceOf[DataBufferInt].getData

    for {
      idy <- 0 until src.getHeight
      dy = idy * src.getWidth
      idx <- 0 until src.getWidth
      i = dy + idx
      if srcPixel(i) == maskcolor
    } {
      srcPixel(i) = targetPixel(i)
    }
    
    src
  }
}
