package pictbliz

import org.scalatest.{OptionValues, BeforeAndAfter, WordSpec, Matchers}

abstract class UnitSpec extends WordSpec with Matchers with BeforeAndAfter with OptionValues

trait ImageSpec {
  import enrich.all._
  type Raw = RawIndexColorImage
  type ImageOp = (Raw, Raw) => Raw

  def testDraw(prefix: String, x: Int, y: Int): Boolean = {
    val (result, expect, _) = testEqualityImages(prefix, { (l, r) =>
      l.drawImage(r, x, y); l
    })("draw")

    result.equalAllPixel(expect)
  }

  def testSynth(prefix: String): Boolean = {
    def isNotAlpha(i: Int) = i.a != 0

    val (result, _, (_, s)) = testEqualityImages(prefix, { (l, r) =>
      l.synthesis(r); l
    })("synth")

    result.testAllPixel(s)(index0AsAlpha = true)(isNotAlpha){
      (l, r) => l == r
    }
  }

  def testTrim(prefix: String, x: Int, y: Int, w: Int, h: Int): Boolean = {
    val (result, expect, _) = testEqualityImages(prefix, { (l, _) =>
      l.trimmed(x, y, w, h)
    })("trim")

    result.equalAllPixel(expect)
  }

  case class PixelTest(prefix: String) {
    def left = ???
    def right = ???
    def expect = ???
    def outputName = ???
  }
  
  def testEqualityImages(prefix: String, f: ImageOp)(implicit testName: String): (Raw, Raw, (Raw, Raw)) = {

    val left = ext.PNG.read(s"testdata/$testName/${prefix}1.png").toRaw
    val right = ext.PNG.read(s"testdata/$testName/${prefix}2.png").toRaw
    val expect = ext.PNG.read(s"testdata/$testName/${prefix}Expect.png").toRaw
    val outputName = s"$testName-${prefix}Dest"
    
    val out = f(left, right)
    ImageResult(outputName, out.toBufferedImage()).write("temp/")
    
    val result = ext.PNG.read(s"temp/$outputName.png").toRaw

    (result, expect, (left, right))
  }
}