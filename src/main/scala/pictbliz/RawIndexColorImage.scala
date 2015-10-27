package pictbliz

import java.awt.image.{DataBuffer, IndexColorModel, BufferedImage}

import scala.collection.mutable

import enrich.bufferedimage._

object RawIndexColorImage {

  final val UNUSED = 0xaaffffff // (alpha = 170) is not appear in IndexColoredImage. Must be 0 or 255.

  final val INIT_COLOR = 0xff000000 // 0xff000000 (non-alpha black)

  def fromBufferedImage(buf: BufferedImage): RawIndexColorImage = {
    val pix = buf.pixelsByte
    val cm = buf.indexColorModel
    val raw = RawIndexColorImage(Array.ofDim[Int](pix.length), Array.ofDim[Int](cm.getMapSize))

    val used = raw.writePixelsWithMarkUsed(pix)
    raw.writePalette(cm)
    raw.markUnusedPalette(used)
    raw
  }

  /* return empty RawIndexColorImage.
     'empty' means its pixels filled index 0, and palette filled INIT_COLOR.
   */
  def fromSize(pixelSize: Int, paletteSize: Int): RawIndexColorImage = {
    val raw = RawIndexColorImage(Array.ofDim[Int](pixelSize), Array.fill(paletteSize)(INIT_COLOR))
    raw.markUnusedPalette(mutable.BitSet(0))
    raw
  }

}

case class RawIndexColorImage private (pixels: Array[Int], palette: Array[Int]) {
  import RawIndexColorImage._

  def color(idx: Int): Int = palette(pixels(idx))

  // TODO: implement extending palette above 256!
  def setColor(idx: Int, color: Int): Unit = {
    findPalette(color) match {
      case Some(pNum) => pixels(idx) = pNum
      case None =>
        findPalette(UNUSED) match {
          case Some(eIdx) =>
            palette(eIdx) = color
            pixels(idx) = eIdx
          case None => sys.error(s"There is no unused palette idx. color: $color")
        }
    }
  }
  
  def length = pixels.length

  def findPalette(targetColor: Int): Option[Int] = {
    val res = palette.indexOf(targetColor)
    if (res != -1) Some(res) else None
  }

  // overwrite palette from IndexColorModel.
  def writePalette(src: IndexColorModel): Unit = {
    require(src.getMapSize == palette.length)

    src.getRGBs(palette)
  }

  // overwrite (int)pixels from (byte)pixels.
  def writePixels(srcPixel: Array[Byte]): Unit = {
    var i = 0
    while(i < srcPixel.length) {
      val idx = srcPixel(i) & 0xff // byte -> Int
      pixels(i) = idx
      i += 1
    }
  }
  
  def writePixelsWithMarkUsed(srcPixel: Array[Byte], used: mutable.BitSet = mutable.BitSet.empty): mutable.BitSet = {
    var i = 0
    while(i < srcPixel.length) {
      val idx = srcPixel(i) & 0xff // byte -> Int
      pixels(i) = idx
      used += idx
      i += 1
    }
    used.result()
  }
  
  def markUnusedPalette(): Unit = {
    val used = mutable.BitSet.empty
    var i = 0
    while(i < pixels.length) {
      val idx = pixels(i) & 0xff
      used += idx
      i += 1
    }
    markUnusedPalette(used)
  }

  def markUnusedPalette(used: mutable.BitSet): Unit = {
    var i = 0
    while(i < palette.length) {
      if (!used(i)) palette(i) = UNUSED
      i += 1
    }
  }

  def restoreUnusedPalette(replaceColor: Int = INIT_COLOR): Unit = {
    var i = 0
    while(i < palette.length) {
      if (palette(i) == UNUSED) palette(i) = replaceColor
      i+=1
    }
  }

  def hasEmptyPalette: Boolean = palette.contains(UNUSED)

  def countEmptyPalette: Int = palette.count(_ == UNUSED)

  def toBufferedImage(width: Int, replaceUnusedPaletteColor: Int = INIT_COLOR): BufferedImage = {
    restoreUnusedPalette(replaceUnusedPaletteColor)

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
