package com.github.whelmaze.pictbliz.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import com.github.whelmaze.pictbliz._
import com.github.whelmaze.pictbliz.scriptops.Attrs._
import java.awt.geom.Rectangle2D

class WrappedGlyphVectorSpec extends WordSpec with ShouldMatchers with BeforeAndAfter {

  val isWindows = {
    val os = Option(System.getProperty("os.name"))
    os map { s =>
      s.toLowerCase.contains("windows")
    } getOrElse false
  }

  def onWindows(f: => Unit) {
    if (isWindows) f
  }

  val alphabets: String =
    ((48 to 57) ++ (65 to 90) ++ (97 to 122)) map { _.toChar } mkString ""

  val amap: AttrMap = Map(
    'a -> ANil,
    'b -> ANil
  )

  def testWrappedGlyphVector(f: WrappedGlyphVector => Unit) {
    val strG = StrGraphics.build(alphabets, amap)
    val wgv = strG.getWrappedGlyphVector
    f(wgv)
    strG.dispose()
  }

  def toTuple4(s: Rectangle2D) = Extractors.Rect2DALL.unapply(s).get

  "WrappedGlyphVector" should {
    "get fixedLogicalBounds correctly" in {
      onWindows {
        testWrappedGlyphVector { wgv: WrappedGlyphVector =>
          val s1 = wgv.getFixedWholeLogicalBounds
          assert(toTuple4(s1) === (0, -11, 372, 13))
          val s2 = wgv.getFixedLogicalBounds(0, 6)
          assert(toTuple4(s2) === (0, -11, 36, 13))
          val s3 = wgv.getFixedLogicalBounds(7, 23)
          assert(toTuple4(s3) === (42, -11, 96, 13))
          val s4 = wgv.getFixedLogicalBounds(22, 23)
          assert(toTuple4(s4) === (132, -11, 6, 13))
          val s5 = wgv.getFixedLogicalBounds(24, 35)
          assert(toTuple4(s5) === (144, -11, 66, 13))
        }
      }
    }
  }
}
