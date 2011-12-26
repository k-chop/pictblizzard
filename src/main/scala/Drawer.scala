package com.github.chuwb.pictbliz

import javax.swing
import java.awt.{ Font, Color, Graphics2D }
import java.awt.font._
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }

import ScriptOps._

object Drawer {
  final val defaultFont = new Font("ＭＳ ゴシック", Font.PLAIN, 13)
}

class Drawer(valuemap: ValueMap, layout: LayoutUnit, context: Context) {
  
  val APoint(sizeX, sizeY) = layout.env('size)
  val img: BufferedImage = new BufferedImage(sizeX, sizeY, BufferedImage.TYPE_INT_ARGB)
  val areamap: AreaMap = layout.areamap

  def draw(): Drawer = {
    clear(img, Color.black)
    areamap foreach { case (name, area) => drawArea(name, area) }
    this
  }

  def drawArea(areaname: Key, areaunit: AreaUnit) = {

    findEnableParam(areaunit.attrmap, 'tile, 'background, 'window) foreach {
      case t: ATile       => drawBackground(t)
      case t: ABackground => drawTile(t)
      case t: AWindow     => drawWindow(t)
    }
    
    val target = valuemap getOrElse (areaname, NullValue)
    target match {
      case Icon(url) => drawIcon(url, areaunit.attrmap)
      case Str(s)  => drawString(s, areaunit.attrmap)
      case NullValue => sys.error("null value! " + target + " in " + areaname)
    }
  }

  private[this] def extractStyle(s: Symbol): Int = s match {
    case 'plain => Font.PLAIN
    case 'bold => Font.BOLD
    case 'italic => Font.ITALIC
    case 'bolditalic => Font.BOLD | Font.ITALIC
    case n => log("不明なフォントスタイル: "+n); Font.PLAIN
  }
  
  def drawString(str: String, attrmap: AttrMap) = {
    import scala.util.control.Exception._

    val font = attrmap.get('font) map { attr =>
      val AFont(name, style, pt, _) = attr
      new Font(name, extractStyle(style), pt.toInt)
    } getOrElse {
      log("フォント設定が見つかりません。デフォルトフォントを使用します。")
      Drawer.defaultFont
    }
    val g2d = img.createGraphics
    val strimg = new StrGraphics(g2d, str, font, attrmap).processImage()
    println("ok.")
    val (px, py) = findBeginPoint(attrmap)
    g2d.drawImage(strimg, null, px, py)  // 横着すんな

    attrmap.get('border) map { _ =>
      g2d.drawRect(px, py, strimg.getWidth, strimg.getHeight)
    }
    
  }

  private[this] def findEnableParam(am: AttrMap, ss: Symbol*): Option[Attr] = ss.toList match {
    case s :: cdr => {
      am find { case(k,v) => k == s } match {
        case Some((k,v)) => Some(v)
        case None => findEnableParam(am, cdr: _*)
      }
    }
    case Nil => None
  }
  
  def findBeginPoint(attrmap: AttrMap): (Int, Int) = {

    findEnableParam(attrmap, 'point, 'rect) map {
      case APoint(x, y)      => (x, y)
      case ARect(x, y, _, _) => (x, y)
    } getOrElse (0, 0)

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
  
  def clear(img: BufferedImage, c: Color) = {
    val g2d = img.createGraphics
    g2d.setPaint(c)
    g2d.fillRect(0, 0, img.getWidth, img.getHeight)
    img
  }

  def write(ref: String) = {
    PNGIO.write(img, ref)
  }
  
}
