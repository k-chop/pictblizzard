package com.github.chuwb.pictbliz

import javax.swing
import java.awt.{ Font, Color, Graphics2D }
import java.awt.font._
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

object Drawer {
  val DEFAULT_FONT = new Font("ＭＳ ゴシック", Font.PLAIN, 13)
}

class DrawableImage(img: BufferedImage) {

  def clear(c: Color) = {
    val g2d = img.createGraphics
    g2d.setPaint(c)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    this
  }

  def drawArea(areaname: Key, areaunit: AreaUnit, target: AValue) = {

    findEnableParam(areaunit.attrmap, 'tile, 'background, 'window) foreach {
      case t: ATile       => drawBackground(t)
      case t: ABackground => drawTile(t)
      case t: AWindow     => drawWindow(t)
    }
    
    target match {
      case Icon(url) => drawIcon(url, areaunit.attrmap)
      case Str(s)  => drawString(s, areaunit.attrmap)
      case NullValue => sys.error("found null value in " + areaname)
    }
    
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

  def drawBackground(attr: Attr) = { }

  def drawTile(attr: Attr) = { }

  def drawWindow(attr: Attr) = { }

  def drawIcon(path: java.net.URI, attrmap: AttrMap) = {
    val g2d = img.createGraphics
    val icon: BufferedImage = PNGIO.read(path)
    val (x,y) = findBeginPoint(attrmap)
    g2d.drawImage(icon, null, x, y)
  }

  private [this] def findBeginPoint(attrmap: AttrMap): (Int, Int) = {
    findEnableParam(attrmap, 'point, 'rect) map {
      case APoint(x, y)      => (x, y)
      case ARect(x, y, _, _) => (x, y)
    } getOrElse (0, 0)
  }
    
  def drawString(str: String, attrmap: AttrMap) = {
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
    val (px, py) = findBeginPoint(attrmap)
    g2d.drawImage(strimg, null, px, py)  // 横着すんな
  }

  private[this] def extractStyle(s: Symbol): Int = s match {
    case 'plain => Font.PLAIN
    case 'bold => Font.BOLD
    case 'italic => Font.ITALIC
    case 'bolditalic => Font.BOLD | Font.ITALIC
    case n => log("不明なフォントスタイル: "+n); Font.PLAIN
  }
  
  def result = img
  def write(ref: String) = PNGIO.write(img, ref)
  
}

class Drawer(layout: LayoutUnit) {

  private[this] val APoint(sizeX, sizeY) = layout.env('size)
  private[this] val areamap: AreaMap = layout.areamap

  def draw(valuemap: ValueMap, context: Context): DrawableImage = {
    val img = new DrawableImage(new BufferedImage(sizeX, sizeY, BufferedImage.TYPE_INT_ARGB))
    img.clear(Color.black)
    areamap foreach {
      case (name, unit) =>
        img.drawArea(name, unit, valuemap getOrElse (name, NullValue))
    }
    img
  }

}

