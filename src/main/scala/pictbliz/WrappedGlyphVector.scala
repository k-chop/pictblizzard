package pictbliz

import java.awt.font.GlyphVector
import java.awt.geom.{ Point2D, Rectangle2D }
import scala.math.{ max, min }

class WrappedGlyphVector(v: GlyphVector, params: Params, newlineCode: Int, val ascent: Float) {
  import Params._

  private[this] val rectAll: Rect =
    params.rect getOrElse Rect(0,0,1,1)
  
  private[this] lazy val codes = (0 until v.getNumGlyphs) map { i => (i, v.getGlyphCode(i)) }
  
  def self = v

  def process(): WrappedGlyphVector = {

    this.newlined()

    params.interval foreach interval
    params.rect foreach (_ => align(params.align))
    params.padding foreach padding

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

  private def interval(in: Interval): WrappedGlyphVector = {
    if (in.x > 0)
      x_interval(in.x)
    if (in.y > 0)
      y_interval(in.y)
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
  
  private def align(ali: Align): WrappedGlyphVector = {

    val Rect(_, _, width, height) = rectAll

    if (ali.x == Align.Right)
      alignXRight(width)
    else if (ali.x == Align.Center)
      alignXCenter(width)

    if (ali.y == Align.Bottom)
      alignYBottom(height)
    else if (ali.y == Align.Center)
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
    val Rect(_, _, w, _) = rectAll
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
    val Rect(_, _, _, h) = rectAll
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

  private def padding(pad: Padding): WrappedGlyphVector = {

    def callPaddings(xp: Align.Horizontal, yp: Align.Vertical) {
      if (pad.x > 0) x_padding(xp, pad.x)
      if (pad.y > 0) y_padding(yp, pad.y)
    }

    if (params.rect.isDefined)
      callPaddings(params.align.x, params.align.y)
    else
      callPaddings(Align.Left, Align.Top)
    
    this
  }
  
  private def x_padding(dir: Align.Horizontal, p: Int) {
    import Align._

    val diff = dir match {
      case Left => p
      case Right => -p
      case Center => 0
    }
    movex(0, v.getNumGlyphs, diff)
  }
  
  private def y_padding(dir: Align.Vertical, p: Int) {
    import Align._

    val diff = dir match {
      case Top => p
      case Bottom => -p
      case Center => 0
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
