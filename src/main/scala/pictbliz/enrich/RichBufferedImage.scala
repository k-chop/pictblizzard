package pictbliz
package enrich

import java.awt.image._

import scala.annotation.tailrec

private[enrich] trait ToRichBufferedImage {
  implicit def toRichBufferedImage(self: BufferedImage): RichBufferedImage = new RichBufferedImage(self)
}

final class RichBufferedImage(val self: BufferedImage) {
  import BufferedImage._
  import enrich.bufferedimage._

  // return 'cloned' RawIndexColorImage.
  def toRaw: RawIndexColorImage = RawIndexColorImage.fromBufferedImage(self)

  // return 'empty' RawIndexColorImage that same size with self.
  def createEmptyDestinationRaw: RawIndexColorImage = {
    val pix = self.pixelsByte
    val cm = self.indexColorModel
    RawIndexColorImage.fromArraySize(pix.length, cm.getMapSize, self.getWidth)
  }

  def drawImageIndexColor(that: BufferedImage, x: Int, y: Int): RawIndexColorImage = {
    val raw = self.toRaw
    raw.drawImage(that.toRaw, x, y)
    raw
  }

  def trim(x: Int, y: Int, w: Int, h: Int): RawIndexColorImage = self.toRaw.trimmed(x, y, w, h)

  def indexColorModel: IndexColorModel = {
    require(self.getType == TYPE_BYTE_INDEXED, s"IndexColorModel defined only TYPE_BYTE_INDEXED. type: ${self.getType}")
    self.getColorModel.asInstanceOf[IndexColorModel]
  }

  def dataType: DataType = self.getType match {
    case TYPE_3BYTE_BGR | TYPE_4BYTE_ABGR | TYPE_4BYTE_ABGR_PRE | TYPE_BYTE_BINARY | TYPE_BYTE_GRAY | TYPE_BYTE_INDEXED => Byte
    case TYPE_INT_ARGB | TYPE_INT_ARGB_PRE | TYPE_INT_BGR | TYPE_INT_RGB => Int
    case TYPE_USHORT_555_RGB | TYPE_USHORT_565_RGB | TYPE_USHORT_GRAY => UShort
  }

  def pixels[T]: T = self.getRaster.getDataBuffer.asInstanceOf[T]

  def pixelsByte: Array[Byte] = self.pixels[DataBufferByte].getData

  def pixelsInt: Array[Int] = self.pixels[DataBufferInt].getData

  def pixelsUShort: Array[Short] = self.pixels[DataBufferUShort].getData

  def compare(that: BufferedImage): Boolean = {
    require(self.getWidth == that.getWidth && self.getHeight == that.getHeight, "'compare' accept only same size")
    require(self.getType == that.getType, "'compare' accept only same type" )

    dataType match {
      case Byte => self.pixelsByte sameElements that.pixelsByte
      case Int => self.pixelsInt sameElements that.pixelsInt
      case UShort => self.pixelsUShort sameElements that.pixelsUShort
    }
  }

  def testAllPixel(that: BufferedImage)(index0AsAlpha: Boolean = false)(withFilter: Int => Boolean)(pred: (Int, Int) => Boolean): Boolean = {
    require(self.getWidth == that.getWidth && self.getHeight == that.getHeight, "'compare' accept only same size")
    require(self.getType == that.getType, "'compareEachPixel' accept only same type")

    val selfRaw = self.toRaw
    val thatRaw = that.toRaw
    
    selfRaw.testAllPixel(thatRaw)(index0AsAlpha)(withFilter)(pred)
  }

}
