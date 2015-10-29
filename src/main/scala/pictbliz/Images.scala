package pictbliz

import java.awt.Color
import java.awt.image.BufferedImage

import com.github.nscala_time.time.StaticDateTimeFormat

import scala.util.Random
import scalaz.Semigroup

import enrich.bufferedimage._

object Images {

  final val origin = (0, 0)

  final def empty = ImageUtils.newImage(1, 1)
  final def emptyPart = ImagePart(origin, empty)
  final def blank(width: Int, height: Int, bgColor: Int = RawIndexColorImage.UNUSED) =
    ImagePart(origin, clear(ImageUtils.newImage(width, height), bgColor))

  def clear(buf: BufferedImage, color: Int): BufferedImage = {
    require(buf.getType == BufferedImage.TYPE_BYTE_INDEXED, "Images#clear accept only index-color")
    val raw = buf.toRaw
    raw.clear(color)
    raw.toBufferedImage(buf.getWidth)
  }

  def findBeginPoint(params: Params, width: Int, height: Int): (Int, Int) = {

    // rewrite!
    params.point match {
      case Some(p) if params.onCenter =>
        ((p.x - width/2.0).toInt, (p.y - height/2.0).toInt)
      case Some(p) => (p.x, p.y)
      case None =>
        params.rect match {
          case Some(r) => (r.x, r.y)
          case None => (0, 0)
        }
    }
  }

}

case class ImageResult(filename: String, image: BufferedImage) {
  import ext.FilePath.ToPath

  def write[T: ToPath](dir: T): Unit = ext.PNG.write(image, implicitly[ToPath[T]].toPath(dir), filename)

}

case class ImagePart(pos: (Int, Int), image: BufferedImage) {
  import com.github.nscala_time.time.Imports._

  private[this] final val fmt = StaticDateTimeFormat.forPattern("yyyyMMddHHmmssSSS")

  def genRandomName: String = DateTime.now().toString(fmt) + Random.alphanumeric.take(6).mkString

  def trimPath(s: String): String = s.replaceAll("""\.png$""", "")

  def result(values: Map[Layouts.Id, Values.Value]): ImageResult = {
    val filename = values.get("filename") match {
      case Some(Values.Text(str)) => trimPath(str)
      case _ => genRandomName
    }
    ImageResult(filename, image)
  }

  def isEmpty = pos._1 == 0 && pos._2 == 0 && image.getWidth == 1 && image.getHeight == 1
}

object ImagePart extends ImagePartInstances

sealed abstract class ImagePartInstances {

  implicit val imagePartInstances: Semigroup[ImagePart] = new Semigroup[ImagePart] {
    import enrich.bufferedimage._

    def append(f1: ImagePart, f2: => ImagePart): ImagePart = {
      if (f1.isEmpty)
        f2
      else if (f2.isEmpty)
        f1
      else {
        val dx = f2.pos._1 - f1.pos._1
        val dy = f2.pos._2 - f1.pos._2

        val raw = f1.image.toRaw
        raw.drawImage(f1.image.getWidth, f2.image.toRaw, f2.image.getWidth, dx, dy)
        val newTo = raw.toBufferedImage(f1.image.getWidth)
        f1.copy(image = newTo)
      }
    }
  }

}