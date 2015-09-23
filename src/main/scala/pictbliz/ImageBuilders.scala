package pictbliz

import scriptops.{Attrs, AttrMap}
import scriptops.Attrs._
import java.awt.image.BufferedImage
import scriptops.{ AttrMap, AttrMapView }


object ImageBuilders {
  import ext.PNG
  import PNG.refconvert._
  import PNG.autobuild._

  implicit val StrBuilder = new Buildable[Str] {
    def build(self: Str, attrmap: AttrMap) = {
/*      val strgraphics = StrGraphics.build(self.s, attrmap)
      val res = strgraphics.processImage()
      strgraphics.dispose()
      ResultImage(findBeginPoint(attrmap, res.getWidth, res.getHeight), res)*/
      ImageBuilder.emptyResult
    }
  }

  implicit val IconBuilder = new Buildable[Icon] {
    def build(self: Icon, attrmap: AttrMap) = {
      val res: BufferedImage = PNG.read(self.uri)
      ResultImage(findBeginPoint(attrmap, res.getWidth, res.getHeight), res)
    }
  }

  implicit val FaceGraphicBuilder = new Buildable[FaceGraphic] {
    def build(self: FaceGraphic, attrmap: AttrMap) = {
      val image = PNG.read(self.uri).transparent(self.transparent)
      val n = self.no
      val res = image.getSubimage((n%4)*48, (n/4)*48, 48, 48)  // 2000用ってことで決め打ち
      ResultImage(findBeginPoint(attrmap, res.getWidth, res.getHeight), res)
    }
  }

  implicit val BattleGraphicBuilder = new Buildable[BattleGraphic] {
    def build(self: BattleGraphic, attrmap: AttrMap) = {
      import self._
      val image = PNG.read(uri).transparent(transparent)
      val res = image.getSubimage(no%4*96, no/4*96, 96, 96)
      ResultImage(findBeginPoint(attrmap, res.getWidth, res.getHeight), res)
    }
  }

  implicit val CharaGraphicBuilder = new Buildable[CharaGraphic] {
    def build(self: CharaGraphic, attrmap: AttrMap) = {
      import self.prop._
      val image = PNG.read(self.uri).transparent(self.transparent)
      val (bx, by) = ((no%4)*72, (no/4)*128)
      val (sx, sy) = (act*24, dir*32)
      val res = image.getSubimage(bx+sx, by+sy, 24, 32)
      ResultImage(findBeginPoint(attrmap, res.getWidth, res.getHeight), res)
    }
  }

  implicit val WindowBuilder = new Buildable[AWindow] {
    def build(self: AWindow, attrmap: AttrMap) = {
      val sysg = SystemGraphics.fromPath(self.systemGraphicsPath)
      val ASize(w, h) = attrmap('size)
      val buf = ImageUtils.newImage(w, h)

      val syswin = sysg.getSystemWindow(w, h, zoom=true)
      val g = buf.createGraphics
      g.drawImage(syswin, null, 0, 0)
      g.dispose()
      ResultImage((0, 0), buf)
    }
  }

  implicit val TileBuilder = new Buildable[ATile] {
    def build(self: ATile, attrmap: AttrMap) = ???
  }

  implicit val BackgroundBuilder = new Buildable[ABackground] {
    def build(self: ABackground, attrmap: AttrMap) = ???
  }

  /**
   * AttrMapの内容から画像の描画位置(左上)を計算する。
   * @param attrmap AttrMap
   * @param width 画像の横幅
   * @param height 画像の縦幅
   * @return 描画位置(左上)
   */
  def findBeginPoint(attrmap: AttrMap, width: Int, height: Int): (Int, Int) = {
    val hasOnCenter = attrmap.contains('oncenter)
    AttrMap.findParam(attrmap, 'point, 'rect) map {
      case APoint(x, y) if hasOnCenter =>
        ((x - width/2.0).toInt, (y - height/2.0).toInt)
      case APoint(x, y)      => (x, y)
      case ARect(x, y, _, _) => (x, y)
    } getOrElse ((0, 0))
  }

}

trait Buildable[T <: Drawable] {
  def build(self: T, attrmap: AttrMap): ResultImage
}

/**
 * 描画位置(左上)と描画するImageを持つ
 */
case class ResultImage(pos: (Int, Int), img: BufferedImage)

// hidoi
object ImageBuilder {
  import ImageBuilders._

  final val emptyResult: ResultImage = ResultImage((0, 0), ImageUtils.newImage(1, 1))

  def build(a: Drawable, m: AttrMap): Option[ResultImage] = {
    Option(a match {
      case s: Str => buildImpl(s, m)
      case i: Icon => buildImpl(i, m)
      case f: FaceGraphic => buildImpl(f, m)
      case c: CharaGraphic => buildImpl(c, m)
      case a: BattleGraphic => buildImpl(a, m)
      case b: ABackground => buildImpl(b, m)
      case t: ATile => buildImpl(t, m)
      case w: AWindow => buildImpl(w, m)
      case n: Number => sys.error(s"Number:$a is not drawable.\nAttrMap ->\n${m.toString}")
      case NullValue => emptyResult
    })
  }

  private def buildImpl[T <: Drawable : Buildable](t: T, attrmap: AttrMap): ResultImage = {
    implicitly[Buildable[T]].build(t, attrmap)
  }

}

