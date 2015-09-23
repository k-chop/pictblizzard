package pictbliz

import java.awt.image.BufferedImage
import java.net.URI

import pictbliz.ext.PNG

object Values {

  trait Value {
    def render(params: Params): ImagePart
  }

  case class Text(str: String) extends Value {

    def render(params: Params): ImagePart = {
      val strgraphics = StrGraphics.build(str, params)
      val res = strgraphics.processImage()
      strgraphics.dispose()
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class Icon(uri: URI) extends Value {
    def render(params: Params): ImagePart = {
      val res: BufferedImage = PNG.read(new java.io.File(uri)).build
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class FaceGraphic(uri: URI, no: Int, transparent: Boolean) extends Value {
    def render(params: Params): ImagePart = {
      val image = PNG.read(new java.io.File(uri)).transparent(transparent).build
      val n = no
      val res = image.getSubimage((n%4)*48, (n/4)*48, 48, 48)  // for rpg maker 2000k
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class CharaGraphic(uri: URI, prop: CharaProperty, transparent: Boolean = true) extends Value {
    def render(params: Params): ImagePart = {
      import prop._
      val image = PNG.read(new java.io.File(uri)).transparent(transparent).build
      val (bx, by) = ((no%4)*72, (no/4)*128)
      val (sx, sy) = (act*24, dir*32)
      val res = image.getSubimage(bx+sx, by+sy, 24, 32)
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class CharaProperty(no: Int, dir: Int, act: Int)

  case class BattleGraphic(uri: URI, no: Int, transparent: Boolean = true) extends Value {
    def render(params: Params): ImagePart = {
      val image = PNG.read(new java.io.File(uri)).transparent(transparent).build
      val res = image.getSubimage(no%4*96, no/4*96, 96, 96)
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  // in Layout
  case class Window(systemGraphicsPath: String) extends Value {

    def render(params: Params): ImagePart = {
      val sysg = SystemGraphics.fromPath(systemGraphicsPath)
      val (w, h) = params.rect.fold((1, 1))(r => (r.w, r.h))
      val buf = ImageUtils.newImage(w, h)

      val syswin = sysg.getSystemWindow(w, h, zoom=true)
      val g = buf.createGraphics
      g.drawImage(syswin, null, 0, 0)
      g.dispose()
      ImagePart((0, 0), buf)
    }
  }


  // etc...
}

abstract class Renderer[T] {


}