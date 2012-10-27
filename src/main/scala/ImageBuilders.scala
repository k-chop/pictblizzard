package com.github.whelmaze.pictbliz

import scriptops.AttrMap
import scriptops.Attrs._
import java.awt.image.BufferedImage
import scriptops.Attrs.AttrMap


object ImageBuilders {

  implicit object StrBuilder extends Buildable[Str] {
    def build(self: Str, attrmap: AttrMap) = {
      val strgraphics = StrGraphics.build(self.s, attrmap)
      val result = strgraphics.processImage()
      strgraphics.dispose()
      ResultImage(findBeginPoint(attrmap), result)
    }
  }

  implicit object IconBuilder extends Buildable[Icon] {
    def build(self: Icon, attrmap: AttrMap): ResultImage = {
      val icon: BufferedImage = ext.PNG.read(self.uri)
      ResultImage(findBeginPoint(attrmap), icon)
    }
  }

  implicit object FaceGraphicBuilder extends Buildable[FaceGraphic] {
    def build(self: FaceGraphic, attrmap: AttrMap): ResultImage = {
      sys.error("not implemented")
    }
  }

  implicit object WindowBuilder extends Buildable[AWindow] {
    def build(self: AWindow, attrmap: AttrMap): ResultImage = {
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

  implicit object TileBuilder extends Buildable[ATile] {
    def build(self: ATile, attrmap: AttrMap): ResultImage = {
      sys.error("not implemented")
    }
  }

  implicit object BackgroundBuilder extends Buildable[ABackground] {
    def build(self: ABackground, attrmap: AttrMap): ResultImage = {
      sys.error("not implemented")
    }
  }

  //util
  def findBeginPoint(attrmap: AttrMap): (Int, Int) = {
    AttrMap.findParam(attrmap, 'point, 'rect) map {
      case APoint(x, y)      => (x, y)
      case ARect(x, y, _, _) => (x, y)
    } getOrElse (0, 0)
  }
}

trait Buildable[T] {
  def build(self: T, attrmap: AttrMap): ResultImage
}

// 描画位置と描画するImageを持つ
case class ResultImage(pos: (Int, Int), img: BufferedImage)

object ImageBuilder {
  import ImageBuilders._

  val emptyResult: ResultImage = ResultImage((0, 0), ImageUtils.newImage(1, 1))

  def build(a: Drawable, m: AttrMap): Option[ResultImage] = {
    Option(a match {
      case s: Str => buildImpl(s, m)
      case i: Icon => buildImpl(i, m)
      case f: FaceGraphic => buildImpl(f, m)
      case b: ABackground => buildImpl(b, m)
      case t: ATile => buildImpl(t, m)
      case w: AWindow => buildImpl(w, m)
      case NullValue => null // Option(null) => None
    })
  }

  private def buildImpl[T : Buildable](t: T, attrmap: AttrMap): ResultImage = {
    implicitly[Buildable[T]].build(t, attrmap)
  }

}

