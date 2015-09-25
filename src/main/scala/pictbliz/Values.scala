package pictbliz

import java.awt.image.BufferedImage

import pictbliz.ext.PNG

object Values {

  trait Value {

    def render(params: Params): ImagePart
  }

  case class Text(str: String) extends Value {

    def render(params: Params): ImagePart = {
      val strGraphics = StrGraphics.build(str, params)
      val res = strGraphics.processImage()
      strGraphics.dispose()
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class Icon(path: String) extends Value {
    def render(params: Params): ImagePart = {
      val res: BufferedImage = PNG.read(path)
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class FaceGraphic(path: String, no: Int, transparent: Boolean) extends Value {
    def render(params: Params): ImagePart = {
      val image = PNG.read(path, transparent = transparent)
      val n = no
      val res = image.getSubimage((n%4)*48, (n/4)*48, 48, 48)  // for rpg maker 2000k
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class CharaGraphic(path: String, prop: CharaProperty, transparent: Boolean = true) extends Value {
    def render(params: Params): ImagePart = {
      import prop._
      val image = PNG.read(path, transparent = transparent)
      val (bx, by) = ((no%4)*72, (no/4)*128)
      val (sx, sy) = (act*24, dir*32)
      val res = image.getSubimage(bx+sx, by+sy, 24, 32)
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class CharaProperty(no: Int, dir: Int, act: Int)

  case class BattleGraphic(path: String, no: Int, transparent: Boolean = true) extends Value {
    def render(params: Params): ImagePart = {
      val image = PNG.read(path, transparent = transparent)
      val res = image.getSubimage(no%4*96, no/4*96, 96, 96)
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  // in Layout
  case class Window(systemGraphicsPath: String) extends Value {

    def render(params: Params): ImagePart = {
      val sysg = SystemGraphics.make(systemGraphicsPath)
      val (w, h) = params.rect.fold((1, 1))(r => (r.w, r.h))
      val buf = ImageUtils.newImage(w, h)

      val systemWindow = sysg.getSystemWindow(w, h, zoom=true)
      val g = buf.createGraphics
      g.drawImage(systemWindow, null, 0, 0)
      g.dispose()
      ImagePart((0, 0), buf)
    }
  }


  // etc...
}

abstract class Renderer[T] {

}