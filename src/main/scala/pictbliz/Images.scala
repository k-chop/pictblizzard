package pictbliz

import java.awt.image.BufferedImage
import java.nio.file.Path

import scalaz.Semigroup

object Images {

  final val origin = (0, 0)

  final def empty = ImageUtils.newImage(1, 1)
  final def emptyPart = ImagePart(origin, empty)
  final def blank(width: Int, height: Int) = ImagePart(origin, ImageUtils.newImage(width, height))

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

  def result(values: Map[Layouts.Id, Values.Value]): ImageResult = {
    val filename = values.get("filename") match {
      case Some(Values.Text(str)) => str
      case _ => "untitled"
    }
    ImageResult(filename, image)
  }
}

object ImagePart extends ImagePartInstances

sealed abstract class ImagePartInstances {

  implicit val imagePartInstances: Semigroup[ImagePart] = new Semigroup[ImagePart] {

    def append(f1: ImagePart, f2: => ImagePart): ImagePart = {
      val (dx, dy) = f2.pos
      val newTo = ImageUtils.copy(f1.image)

      val g2d = newTo.createGraphics()
      g2d.drawImage(f2.image, null, dx, dy)
      g2d.dispose()
      f1.copy(image = newTo)
    }
  }

}