package pictbliz

import org.scalatest.{OptionValues, BeforeAndAfter, WordSpec, Matchers}

import scala.annotation.tailrec

abstract class UnitSpec extends WordSpec with Matchers with BeforeAndAfter with OptionValues

trait ImageSpec {
  this: UnitSpec =>

  import enrich.all._

  type Raw = RawIndexColorImage

  def testDraw(l: String, r: String, e: String, x: Int, y: Int): Unit = {
    val t = PixelTest("draw", l, r, e)
    val res = t.test { import t._
      left.drawImage(right, x, y); left
    }
    equalAllPixel(res, t.expect) shouldBe true
  }

  def testSynth(l: String, r: String): Unit = {
    val t = PixelTest("synth", l, r)
    val res = t.test { import t._
      left.synthesis(right); left
    }
    testAllPixel(res, t.right)(index0AsAlpha = true)(_.a != 0){
      (l, r) => l === r
    } shouldBe true
  }

  def testTrim(l: String, e: String, x: Int, y: Int, w: Int, h: Int): Unit = {
    val t = PixelTest("trim", l, "", e)
    val res = t.test { import t._
      left.trimmed(x, y, w, h)
    }
    equalAllPixel(res, t.expect) shouldBe true
  }

  @inline final def testAllPixel(self: Raw, that: Raw)(index0AsAlpha: Boolean = false)(withFilter: Int => Boolean)(pred: (Int, Int) => Boolean): Boolean = {
    @tailrec def rec(idx: Int = 0, ret: Boolean = true): Boolean = if (self.length <= idx) ret
    else {
      val cs = self.color(idx, index0AsAlpha)
      if (withFilter(cs)) {
        val co = that.color(idx, index0AsAlpha)
        if (!pred(cs, co)) {
          false
        } else rec(idx + 1, ret)
      } else rec(idx + 1, ret)
    }
    assert(self.pixels.length == that.pixels.length)
    rec()
  }

  @inline final def equalAllPixel(self: Raw, that: Raw) =
    testAllPixel(self, that)(index0AsAlpha = true)(_ => true)( (l, r) => { assert(l === r); l === r } )

  case class PixelTest(testName: String, _left: String, _right: String = "", _expect: String = "") {
    lazy val left = ext.PNG.read(s"testdata/$testName/${_left}.png").toRaw
    lazy val right = ext.PNG.read(s"testdata/$testName/${_right}.png").toRaw
    lazy val expect = ext.PNG.read(s"testdata/$testName/${_expect}.png").toRaw
    lazy val result = ext.PNG.read(s"temp/$outputName.png").toRaw
    private val outputName = s"$testName-${_left}-${_right}-${_expect}"
    def test(f: => Raw): Raw = {
      val out = f
      ImageResult(outputName, out.toBufferedImage()).write("temp/")
      ext.PNG.read(s"temp/$outputName.png").toRaw
    }
  }

}