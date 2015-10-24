package pictbliz
package enrich

import java.awt.image._

import scala.annotation.tailrec

private[enrich] trait ToRichBufferedImage extends RichBufferedImageInstance

sealed abstract class RichBufferedImageInstance {

  implicit final class RichBufferedImage(val self: BufferedImage) extends AnyVal {
    import BufferedImage._

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

    def compareEachPixel(that: BufferedImage)(withFilter: Int => Boolean)(pred: (Int, Int) => Boolean) = {
      require(self.getWidth == that.getWidth && self.getHeight == that.getHeight, "'compare' accept only same size")
      require(self.getType == that.getType, "'compareEachPixel' accept only same type")

      val selfPix = self.pixelsByte
      val thatPix = self.pixelsByte
      val selfCM = self.indexColorModel
      val thatCM = that.indexColorModel

      @tailrec def rec(idx: Int = 0, ret: Boolean = true): Boolean = if (idx < selfPix.length) ret
      else {
        val cs = selfCM.getRGB(selfPix(idx))
        if (withFilter(cs)) {
          val co = thatCM.getRGB(thatPix(idx))
          if (!pred(cs, co)) false else rec(idx + 1, ret)
        } else rec(idx + 1, ret)
      }
      rec()
    }
  }
}
