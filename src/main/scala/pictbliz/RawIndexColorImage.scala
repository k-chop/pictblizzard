package pictbliz

import java.awt.image.{DataBuffer, IndexColorModel, BufferedImage}

import scala.collection.mutable

import enrich.bufferedimage._

object RawIndexColorImage {

  final val UNUSED = 0x00ffffff // (alpha = 0) is not appear with IndexColoredImage.

  def fromBufferedImage(buf: BufferedImage): RawIndexColorImage = {
    val pix = buf.pixelsByte
    val cm = buf.indexColorModel
    val raw = RawIndexColorImage(Array.ofDim[Int](pix.length), Array.ofDim[Int](cm.getMapSize))
    raw.writePalette(cm)
    raw.writePixels(pix)
    raw
  }
}

case class RawIndexColorImage(pixels: Array[Int], palette: Array[Int]) {
  import RawIndexColorImage._

  def color(idx: Int): Int = palette(pixels(idx))

  def length = pixels.length

  def findPalette(target: Int): Option[Int] = {
    val res = palette.indexOf(target)
    if (res != -1) Some(res) else None
  }

  // overwrite palette from IndexColorModel.
  def writePalette(src: IndexColorModel): Unit = {
    require(src.getMapSize == palette.length)

    src.getRGBs(palette)
  }

  def writePixels(src: BufferedImage): Unit = writePixels(src.pixelsByte)

  // overwrite (int)pixels from (byte)pixels.
  def writePixels(srcPixel: Array[Byte]): Unit = {
    var i = 0
    while(i < srcPixel.length) {
      val idx = srcPixel(i) & 0xff // byte -> Int
      pixels(i) = idx
      i += 1
    }
  }

  def writePixelsWithMarkUsed(src: BufferedImage, used: mutable.BitSet = mutable.BitSet.empty): mutable.BitSet = {
    val srcPixel = src.pixelsByte
    var i = 0
    while(i < srcPixel.length) {
      val idx = srcPixel(i) & 0xff // byte -> Int
      pixels(i) = idx
      used += idx
      i += 1
    }
    used.result()
  }

  def markUnusedPalette(used: mutable.BitSet): Unit = {
    var i = 0
    while(i < palette.length) {
      if (!used(i)) palette(i) = UNUSED
      i += 1
    }
  }

  def restoreUnusedPalette(replaceColor: Int = 0x00): Unit = {
    var i = 0
    while(i < palette.length) {
      if (palette(i) == UNUSED) palette(i) = replaceColor
      i+=1
    }
  }

  def toBufferedImage(width: Int): BufferedImage = {
    val cm = new IndexColorModel(8, palette.length, palette, 0, true, 0,  DataBuffer.TYPE_BYTE)
    val buf = new BufferedImage(width, pixels.length / width, BufferedImage.TYPE_BYTE_INDEXED, cm)

    val pix = buf.pixelsByte
    var i = 0
    while(i < pix.length) {
      pix(i) = pixels(i).toByte
      i += 1
    }
    buf
  }

  @inline final def foreachWithIndex[A](f: Int => A): Unit = {
    var i = 0
    while(i < pixels.length) {
      f(i)
      i += 1
    }
  }

}
