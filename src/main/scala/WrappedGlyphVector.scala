package pictbliz

import java.awt.font.GlyphVector
import java.awt.geom.{ Point2D, Rectangle2D }
import scala.math.{ max, min }

import scriptops.AttrMap
import scriptops.Attrs._


class WrappedGlyphVector(v: GlyphVector, attrmap: AttrMap, newlineCode: Int, val ascent: Float) {

  private[this] val rectAll: ARect =
    attrmap.get('rect) collect {
      case r: ARect => r
    } getOrElse ARect(0,0,1,1)
  
  private[this] lazy val codes = (0 until v.getNumGlyphs) map { i => (i, v.getGlyphCode(i)) }
  
  def self = v

  def process(): WrappedGlyphVector = {
    def call(pname: Symbol, f: Attr => WrappedGlyphVector, guard: => Boolean = true) {
      if (guard) attrmap.get(pname) foreach { f }
    }
    
    this.newlined()

    call('interval, interval)
    call('align,    align,   attrmap.contains('rect))
    call('padding,  padding)
    
    this
  }

  def newlined(): WrappedGlyphVector = {
    var lineLeaderIdx = 0
    (1 until v.getNumGlyphs) foreach { idx =>
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

  private def interval(attr: Attr): WrappedGlyphVector = {
    val AInterval(xparam, yparam) = attr
    if (xparam > 0)
      x_interval(xparam)
    if (yparam > 0)
      y_interval(yparam)
    this
  }
  
  private def x_interval(p: Int) {
    (1 until v.getNumGlyphs) foreach { idx =>
      if (v.getGlyphCode(idx - 1) != newlineCode) { // 前が改行の場合移動する必要なし
        val rect = v.getGlyphLogicalBounds(idx - 1).getBounds
        val oldpos = v.getGlyphPosition(idx)
        val newpos = new Point2D.Double(rect.getWidth + p + rect.getX, oldpos.getY)
        v.setGlyphPosition(idx, newpos)
      }
    }
  }

  private def y_interval(p: Int) {
    var nlcount = 0
    
    (1 until v.getNumGlyphs) foreach { idx =>
      if (v.getGlyphCode(idx - 1) == newlineCode)
        nlcount += 1
      val oldpos = v.getGlyphPosition(idx)
      val newpos = new Point2D.Double(oldpos.getX, oldpos.getY + (p * nlcount))
      v.setGlyphPosition(idx, newpos)
    }
  }
  
  private def align(attr: Attr): WrappedGlyphVector = {
    val AAlign(xparam, yparam) = attr
    
    val ARect(_, _, width, height) = rectAll

    if (xparam == 'right)
      alignXRight(width)
    else if (xparam == 'x_center)
      alignXCenter(width)

    if (yparam == 'bottom)
      alignYBottom(height)
    else if (yparam == 'y_center)
      alignYCenter(height)
    
    this
  }

  private def movex(begin: Int, end: Int, diffx: Double) {
    (begin until end) foreach { idx =>
      val oldpos = v.getGlyphPosition(idx)
      val newpos = new Point2D.Double(oldpos.getX + diffx, oldpos.getY)
      v.setGlyphPosition(idx, newpos)        
    }
  }
  
  private def alignXRight(width: Int) {
    
    def recur(lf: Int, nl: Int) {
      if (nl != -1) {
        val lp = v.getGlyphPosition(nl)
        val diff = width - lp.getX
        if (diff > 0) movex(lf, nl + 1, diff)
        recur(nl + 1, nextNewline(nl + 1))
      } else {
        val lp = v.getGlyphLogicalBounds(v.getNumGlyphs - 1).getBounds
        val diff = width - (lp.getX + lp.getWidth)
        if (diff > 0) movex(lf, v.getNumGlyphs, diff)        
      }
    }
    
    recur(0, nextNewline(0))
  }

  private def alignXCenter(width: Int) {
    val ARect(_, _, w, h) = rectAll
    def recur(lf: Int, nl: Int) {
      if (nl != -1) {
        val lineRect = getFixedLogicalBounds(lf, nl+1)
        val diff = (w - lineRect.getWidth) / 2
        if (diff > 0) movex(lf, nl + 1, diff)
        recur(nl + 1, nextNewline(nl + 1))
      } else {
        val lineRect = getFixedLogicalBounds(lf, v.getNumGlyphs)
        val diff = (w - lineRect.getWidth) / 2
        if (diff > 0) movex(lf, v.getNumGlyphs, diff)
      }
    }
    
    recur(0, nextNewline(0))
  }

  private def movey(begin: Int, end: Int, diffy: Double) {
    (begin until end) foreach { idx =>
      val oldpos = v.getGlyphPosition(idx)
      val newpos = new Point2D.Double(oldpos.getX, oldpos.getY + diffy)
      v.setGlyphPosition(idx, newpos)        
    }
  }
  
  private def alignYBottom(height: Int) {
    val glb = v.getGlyphLogicalBounds(v.getNumGlyphs - 1).getBounds
    val gp = v.getGlyphPosition(v.getNumGlyphs - 1)
    val diff = height - (gp.getY + glb.getHeight)

    if (diff > 0) movey(0, v.getNumGlyphs, diff)
  }

  private def alignYCenter(height: Int) {
    val ARect(_, _, _, h) = rectAll
    val fixedRect = getFixedLogicalBounds(0, v.getNumGlyphs)
    val diff = (h - fixedRect.getHeight) / 2
    
    if (diff > 0) movey(0, v.getNumGlyphs, diff)
  }

  /**
  * 次の改行の位置をさがす。なければ-1を返すよ。Optionにした方がよくね
  */
  private def nextNewline(from: Int): Int = {
    (codes.view drop from find { _._2 == newlineCode } getOrElse((-1,0)) )._1
  }

  private def padding(attr: Attr): WrappedGlyphVector = {

    val APadding(xparam, yparam) = attr
    
    def callPaddings(xp: Symbol, yp: Symbol) {
      if (xparam > 0) x_padding(xp, xparam)
      if (yparam > 0) y_padding(yp, yparam)
    }

    if (attrmap.contains('rect) && attrmap.contains('align)) {
      val AAlign(hori, vert) = attrmap('align)
      callPaddings(hori, vert)
    } else if (attrmap.contains('rect)) {
      callPaddings('left, 'top)
    } else if (attrmap.contains('point)) {
      callPaddings('left, 'top)
    } else sys.error("なんという条件漏れ… : " + attrmap.toString)
    
    this
  }
  
  private def x_padding(dir: Symbol, p: Int) {
    val diff = dir match {
      case 'left => p
      case 'right => -p
      case 'x_center => 0
    }
    movex(0, v.getNumGlyphs, diff)
  }
  
  private def y_padding(dir: Symbol, p: Int) {
    val diff = dir match {
      case 'top => p
      case 'bottom => -p
      case 'y_center => 0
    }
    movey(0, v.getNumGlyphs, diff)
  }
  
  def getFixedWholeLogicalBounds: Rectangle2D = getFixedLogicalBounds(0, v.getNumGlyphs)
  
  /** 
  * GlyphVector#getLogicalBoundsはsetGlyphPositionで
  * Glyphの位置を動かした後だと正しい位置を返してくれないので自前で計算する。
  * [begin, end)間のすべてのGlyphを完全に含む矩形を返す。
  */
  def getFixedLogicalBounds(begin: Int, end: Int): Rectangle2D = {
    require(begin <= end, "begin(%d) must be equal or less than end(%d)" format (begin, end))
    require(
      0 <= begin && end <= v.getNumGlyphs,
      "0 <= begin(%d) && end(%d) < %d(numGlyphs)" format (begin, end, v.getNumGlyphs)
    )

    var idx = begin
    val init = (Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue)
    var (lx, ly, rx, ry) = init
    while(idx < end) {
      val r = v.getGlyphLogicalBounds(idx).getBounds
      lx = min(lx, r.getX)
      ly = min(ly, r.getY)
      rx = max(rx, r.getX + r.getWidth)
      ry = max(ry, r.getY + r.getHeight)
      idx += 1
    }
    if ((lx, ly, rx, ry) == init)
      new Rectangle2D.Double(0, 0, 1, 1)
    else
      new Rectangle2D.Double(lx, ly, rx-lx, ry-ly)
  }

}
