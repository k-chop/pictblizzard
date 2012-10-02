package com.github.whelmaze.pictbliz.ext

import java.io.FileInputStream
import java.nio.channels.FileChannel
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import com.github.whelmaze.pictbliz.{BinaryUtils, ImageUtils}

object PNG {
  
  def read(path: java.net.URI): BufferedImage = ImageIO.read(new File(path))

  def read(path: String): BufferedImage = read( (new File(path)).toURI )
  
  def readAsARGBImage(path: java.net.URI): BufferedImage = 
    ImageUtils.toARGBImage( read(path) )
  
  def readAsARGBImage(path: String): BufferedImage = 
    readAsARGBImage( (new File(path)).toURI )
  
  def write(img: BufferedImage, path: String) {
    ImageIO.write(img, "png", new File(path))
  }
  
  import BinaryUtils._

  private[this] val DWORD = new Array[Byte](4)
  private[this] val QWORD = new Array[Byte](8)

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
      while (map.hasRemaining) {
        map.get(DWORD)
        val len = DWORD2Long(DWORD)
        map.get(DWORD)
        val name = DWORD map { _.toChar } mkString ""
        if (name == "PLTE") {
          val r = map.get
          val g = map.get
          val b = map.get
          result = unsign(b) | (unsign(g) << 8) | (unsign(r) << 16) | 0xff000000 & 0xffffffff
          map.position(map.position - 3 + len.toInt + 4)
        } else {
          map.position(map.position + len.toInt + 4)
        }
      }
      result
    } finally {
      cnl.close()
    }

  }

}
