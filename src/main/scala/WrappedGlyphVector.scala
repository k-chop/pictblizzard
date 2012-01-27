package com.github.chuwb.pictbliz

import java.awt.{ Font, Color, Graphics2D }
import java.awt.image.{ BufferedImage }
import javax.imageio.{ ImageIO }
import java.io.{ File }
import java.awt.font.{ GlyphVector }
import java.awt.geom.{ Point2D, Rectangle2D }

import ScriptOps._

object WrappedGlyphVector {
  
  object implicits {
    implicit def wrapped2rawGlyphVector(v: WrappedGlyphVector) = v.self
  }

}

class WrappedGlyphVector(val v: GlyphVector, attrmap: AttrMap, newlineCode: Int, val ascent: Float) {

  private[this] val rectAll: ARect =
    attrmap.get('rect) collect {
      case r: ARect => r
    } getOrElse ARect(0,0,1,1)
  
  private[this] lazy val codes = (0 until v.getNumGlyphs) map { i => (i, v.getGlyphCode(i)) }
  
  def self = v

  def process(): WrappedGlyphVector = {
    def call(pname: Symbol, f: Attr => WrappedGlyphVector, guard: => Boolean = true) =
      if (guard) attrmap.get(pname) foreach { f }
    
    this.newlined()

    call('x_interval, x_interval)
    call('y_interval, y_interval)
    call('align,      align     , attrmap.contains('rect))
    call('padding,    padding   )

    this
  }

  def newlined(): WrappedGlyphVector = {
    var lineLeaderIdx = 0
    for (idx <- 1 until v.getNumGlyphs) {
      if (v.getGlyphCode(idx - 1) == newlineCode) { // 改行
        val rect = v.getGlyphLogicalBounds(lineLeaderIdx).getBounds
        val oldpos = v.getGlyphPosition(lineLeaderIdx)
        val newpos = new Point2D.Double(oldpos.getX, oldpos.getY + rect.getHeight)
        v.setGlyphPosition(idx, newpos)
        lineLeaderIdx = idx
      } else { // 前の文字の次に設置
        val rect = v.getGlyphLogicalBounds(idx - 1).getBounds
        val oldpos = v.getGlyphPosition(idx - 1)
        val newpos = new Point2D.Double(rect.getWidth + oldpos.getX, oldpos.getY)
        v.setGlyphPosition(idx, newpos)
      }
    }
    this
  }
  
  private[this] def x_interval(attr: Attr): WrappedGlyphVector = {
    val AXInterval(p) = attr

    for (idx <- 1 until v.getNumGlyphs) {
      if (v.getGlyphCode(idx - 1) != newlineCode) { // 前が改行の場合移動する必要なし
        val rect = v.getGlyphLogicalBounds(idx - 1).getBounds
        val oldpos = v.getGlyphPosition(idx)
        val newpos = new Point2D.Double(rect.getWidth + p + rect.getX, oldpos.getY)
        v.setGlyphPosition(idx, newpos)
      }
    }
    this
  }

  private[this] def y_interval(attr: Attr): WrappedGlyphVector = {
    val AYInterval(p) = attr
    var nlcount = 0
    
    for (idx <- 1 until v.getNumGlyphs) {
      if (v.getGlyphCode(idx - 1) == newlineCode)
        nlcount += 1
      val oldpos = v.getGlyphPosition(idx)
      val newpos = new Point2D.Double(oldpos.getX, oldpos.getY + (p * nlcount))
      v.setGlyphPosition(idx, newpos)
    }
    this
  }
  
  private[this] def align(attr: Attr): WrappedGlyphVector = {
    val AAlign(xparam, yparam) = attr
    
    val ARect(_, _, width, height) = rectAll

    if ( xparam == 'right)
      alignXRight(width)
    else if ( xparam == 'x_center)
      alignXCenter(width)

    if ( yparam == 'bottom)
      alignYBottom(height)
    else if ( yparam == 'y_center)
      alignYCenter(height)
    
    this
  }

  private[this] def movex(begin: Int, end: Int, diffx: Double) = {
    for (idx <- begin to end) {
      val oldpos = v.getGlyphPosition(idx)
      val newpos = new Point2D.Double(oldpos.getX + diffx, oldpos.getY)
      v.setGlyphPosition(idx, newpos)        
    }
  }
  
  private[this] def alignXRight(width: Int) = {
    
    def recur(lf: Int, nl: Int) {
      if (nl != -1) {
        val lp = v.getGlyphPosition(nl)
        val diff = width - lp.getX
        if (diff > 0) movex(lf, nl, diff)
        recur(nl + 1, nextNewline(nl + 1))
      } else {
        val lp = v.getGlyphLogicalBounds(v.getNumGlyphs - 1).getBounds
        val diff = width - (lp.getX + lp.getWidth)
        if (diff > 0) movex(lf, v.getNumGlyphs - 1, diff)        
      }
    }
    
    recur(0, nextNewline(0))
  }

  private[this] def alignXCenter(width: Int) = {
    val ARect(_, _, w, h) = rectAll
    def recur(lf: Int, nl: Int) {
      if (nl != -1) {
        val lineRect = getFixedLogicalBounds(lf, nl+1)
        val diff = (w - lineRect.getWidth) / 2
        if (diff > 0) movex(lf, nl, diff)
        recur(nl + 1, nextNewline(nl + 1))
      } else {
        val lineRect = getFixedLogicalBounds(lf, v.getNumGlyphs)
        val diff = (w - lineRect.getWidth) / 2
        if (diff > 0) movex(lf, v.getNumGlyphs-1, diff)
      }
    }
    
    recur(0, nextNewline(0))
  }

  private[this] def movey(begin: Int, end: Int, diffy: Double) = {
    for (idx <- begin to end) {
      val oldpos = v.getGlyphPosition(idx)
      val newpos = new Point2D.Double(oldpos.getX, oldpos.getY + diffy)
      v.setGlyphPosition(idx, newpos)        
    }
  }
  
  private[this] def alignYBottom(height: Int) = {
    val glb = v.getGlyphLogicalBounds(v.getNumGlyphs - 1).getBounds
    val gp = v.getGlyphPosition(v.getNumGlyphs - 1)
    val diff = height - (gp.getY + glb.getHeight)

    if (diff > 0) movey(0, v.getNumGlyphs - 1, diff)
  }

  private[this] def alignYCenter(height: Int) = {
    val ARect(_, _, _, h) = rectAll
    val fixedRect = getFixedLogicalBounds(0, v.getNumGlyphs)
    val diff = (h - fixedRect.getHeight) / 2
    
    if (diff > 0) movey(0, v.getNumGlyphs - 1, diff)
  }
  
  /** 
  * 次の改行の位置をさがす。なければ-1を返すよ。Optionにした方がよくね
  */ 
  private[this] def nextNewline(from: Int): Int = {    
    (codes.view drop (from) find { _._2 == newlineCode } getOrElse((-1,0)) )._1
  }

  /*
   * 指定方向の余白を決める。
   * 正直いらない気がする。気が向いたら実装
   */
  private[this] def padding(attr: Attr): WrappedGlyphVector = {
    this
  }

  def getFixedLogicalBounds(): Rectangle2D = getFixedLogicalBounds(0, v.getNumGlyphs)
  
  /** 
  * GlyphVector#getLogicalBoundsはsetGlyphPositionで
  * Glyphの位置を動かした後だと正しい位置を返してくれないので自前で計算する。
  * [begin, end)間のすべてのGlyphを完全に含む矩形を返す。
  * 実装がひどい、これもうちょっとどうにかする
  */ 
  def getFixedLogicalBounds(begin: Int, end: Int): Rectangle2D = {
    import scala.math.{ max, min }
    val init = (Double.MaxValue, Double.MaxValue,
                Double.MinValue, Double.MinValue)
    
    (begin until end map { idx: Int =>
      val rect = v.getGlyphLogicalBounds(idx).getBounds
      (rect.getX, rect.getY, rect.getX + rect.getWidth, rect.getY + rect.getHeight)
    }).foldLeft(init){ case ((oa,ob,oc,od),(na,nb,nc,nd)) =>
      (min(oa,na), min(ob,nb), max(oc,nc), max(od,nd))
    } match {
      case `init` =>
        new Rectangle2D.Double(0, 0, 1, 1) // minimum
      case (x1, y1, x2, y2) =>
        new Rectangle2D.Double(x1, y1, x2-x1, y2-y1)
    }    
  }

}
