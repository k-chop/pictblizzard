package pictbliz

import org.scalatest.{OptionValues, BeforeAndAfter, WordSpec, Matchers}

abstract class UnitSpec extends WordSpec with Matchers with BeforeAndAfter with OptionValues

trait ImageSpec {
  import enrich.all._

  def testEqualityAllPixelWithDraw
  (
      prefix: String,
      x: Int = 0,
      y: Int = 0
      )(implicit testName: String): Boolean = {

    val l = ext.PNG.read(s"testdata/$testName/${prefix}1.png")
    val r = ext.PNG.read(s"testdata/$testName/${prefix}2.png")
    val e = ext.PNG.read(s"testdata/$testName/${prefix}Expect.png")

    val raw = l.drawImageIndexColor(r, x, y)
    val buf = raw.toBufferedImage()
    val outputName = s"$testName-${prefix}Dest"
    ImageResult(outputName, buf).write("temp/")

    val res = ext.PNG.read(s"temp/$outputName.png")
    res.testAllPixel(e)(index0AsAlpha = true)(_ => true){
      (l, r) => l == r
    }
  }
}