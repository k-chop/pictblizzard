package com.github.chuwb.pictbliz

import javax.swing
import java.awt.{ Font, Color, Graphics2D }
import java.awt.font._
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

object Drawer {
  val DEFAULT_FONT = new Font("ＭＳ ゴシック", Font.PLAIN, 12)
}

class Drawer(layout: LayoutUnit) {

  private[this] val APoint(sizeX, sizeY) = layout.env('size)
  private[this] val areamap: AreaMap = layout.areamap

  def draw(valuemap: ValueMap, context: Context): DrawableImage = {
    val img = new DrawableImage(ImageUtils.newImage(sizeX, sizeY))
    img.clear(Color.black)
    areamap foreach {
      case (name, unit) =>
        img.drawArea(name, unit, valuemap getOrElse (name, NullValue))
    }
    img
  }

}


class DrawableImage(img: BufferedImage) {

  case class ResultImage(pos: (Int, Int), img: BufferedImage)
  
  def clear(c: Color) = {
    val g2d = img.createGraphics
    g2d.setPaint(c)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    g2d.dispose
    this
  }

  def drawArea(areaname: Key, areaunit: AreaUnit, target: AValue) = {

    val frontImage = Option(target) map {
      case Icon(url) =>
        iconImage(url, areaunit.attrmap)
      case Str(s)  =>
        stringImage(s, areaunit.attrmap)
      case NullValue =>
        null // Option(null) => None
    }

    val (fx, fy, exSize) = frontImage map {
      case ResultImage((x, y), target) => (x, y, (target.getWidth, target.getHeight))
    } getOrElse((0, 0, (1, 1)))
    
    val bgkind = findEnableParam(areaunit.attrmap, 'tile, 'background, 'window)
    val bgImage = bgkind map {
      case t: ATile       => tileImage(exSize, t)
      case t: ABackground => backgroundImage(exSize, t)
      case t: AWindow     => windowImage(exSize, t)
    }

    
    val g = img.createGraphics
    bgImage foreach {
      case ResultImage(_, target) => g.drawImage(target, null, fx, fy)
    }
    frontImage foreach {
      case ResultImage((x, y), target) => g.drawImage(target, null, x, y)
    }
    g.dispose
  }

  private[this] def findEnableParam(am: AttrMap, ss: Symbol*): Option[Attr] = {
    
    @scala.annotation.tailrec
    def findEnableParamRec(am: AttrMap, sl: List[Symbol]): Option[Attr] = sl match {
      case s :: cdr => {
        am find { case(k,v) => k == s } match {
          case Some((k,v)) => Some(v)
          case None => findEnableParamRec(am, cdr)
        }
      }
      case Nil => None
    }
    findEnableParamRec(am, ss.toList)
  }

  def backgroundImage(size: (Int, Int), attr: Attr): ResultImage = null

  def tileImage(size: (Int, Int), attr: Attr): ResultImage = null

  def windowImage(size: (Int, Int), attr: Attr): ResultImage = {
    // このへんでAttrMap/Context読み取って適切なSystemGraphicsをげっちゅ 今は仮
    val sysg = SystemGraphics.default
    val buf = ImageUtils.newImage(size)

    val (sw, sh) = size
    val syswin = sysg.getSystemWindow(sw, sh, zoom=true)
    val g = buf.createGraphics
    g.drawImage(syswin, null, 0, 0)
    g.dispose
    ResultImage((0, 0), buf)
  }

  def iconImage(path: java.net.URI, attrmap: AttrMap): ResultImage = {
    val icon: BufferedImage = ext.PNG.read(path)
    ResultImage(findBeginPoint(attrmap), icon)
  }

  private [this] def findBeginPoint(attrmap: AttrMap): (Int, Int) = {
    findEnableParam(attrmap, 'point, 'rect) map {
      case APoint(x, y)      => (x, y)
      case ARect(x, y, _, _) => (x, y)
    } getOrElse (0, 0)
  }

  def stringImage(str: String, attrmap: AttrMap): ResultImage = {
    import scala.util.control.Exception._

    val font = attrmap.get('font) map { attr =>
      val AFont(name, style, pt, _) = attr
      new Font(name, extractStyle(style), pt.toInt)
    } getOrElse {
      log("フォント設定が見つかりません。デフォルトフォントを使用します。")
      Drawer.DEFAULT_FONT
    }

    val g2d = img.createGraphics
    val strimg = new StrGraphics(g2d, str, font, attrmap).processImage()
    g2d.dispose
    ResultImage(findBeginPoint(attrmap), strimg)
  }

  private[this] def extractStyle(s: Symbol): Int = s match {
    case 'plain => Font.PLAIN
    case 'bold => Font.BOLD
    case 'italic => Font.ITALIC
    case 'bolditalic => Font.BOLD | Font.ITALIC
    case n => log("不明なフォントスタイル: "+n); Font.PLAIN
  }
  
  def result = img
  def write(ref: String) = ext.PNG.write(img, ref)
  
}

