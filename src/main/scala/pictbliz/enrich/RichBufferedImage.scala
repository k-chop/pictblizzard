package pictbliz
package enrich

import java.awt.image._

import scala.annotation.tailrec

trait ToRichBufferedImage extends RichBufferedImageInstance

abstract class RichBufferedImageInstance {

  implicit def ToRichBufferedImage(self: BufferedImage): RichBufferedImage = new RichBufferedImage(self)
}

final class RichBufferedImage(val self: BufferedImage) {
  import BufferedImage._
  import enrich.bufferedimage._

  // return 'empty' RawIndexColorImage that same size with self.
  def createRawIndexColorImage: RawIndexColorImage = {
    val pix = self.pixelsByte
    val cm = self.indexColorModel
    RawIndexColorImage(Array.ofDim[Int](pix.length), Array.ofDim[Int](cm.getMapSize))
  }

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

  def forallPixel(that: BufferedImage)(withFilter: Int => Boolean)(pred: (Int, Int) => Boolean): Boolean = {
    require(self.getWidth == that.getWidth && self.getHeight == that.getHeight, "'compare' accept only same size")
    require(self.getType == that.getType, "'compareEachPixel' accept only same type")

    val selfPix = self.pixelsByte
    val thatPix = that.pixelsByte
    val selfCM = self.indexColorModel
    val thatCM = that.indexColorModel

    @tailrec def rec(idx: Int = 0, ret: Boolean = true): Boolean = if (selfPix.length <= idx) ret
    else {
      val cs = selfCM.getRGB(selfPix(idx))
      if (withFilter(cs)) {
        val co = thatCM.getRGB(thatPix(idx))
        if (!pred(cs, co)) {
          println(s"at $idx, $cs and $co do not satisfy predicate")
          false
        } else rec(idx + 1, ret)
      } else rec(idx + 1, ret)
    }
    rec()
  }
}
