package pictbliz

import RawIndexColorImage._
import enrich.bufferedimage._
import enrich.packedcolor._

class RawIndexColorImageSpec extends UnitSpec {

  "RawIndexColorImage" should {

    def rawFixture = RawIndexColorImage.fromSize(5, 0xff)

    "create empty RawIndexColorImage from fromSize" in {
      val raw = rawFixture
      raw.countPalette shouldEqual 1
      raw.palette(0) shouldBe INIT_COLOR
      all (raw.pixels) should equal (0)
    }

    "set color with extending palette" in {
      val raw = rawFixture
      val nc = 0xffeeeeee
      raw.setColor(0, nc)
      raw.countPalette shouldEqual 2
      raw.findPalette(nc).value shouldEqual 1
      raw.palette should contain (nc)
    }

    "set color without extending palette" in {
      val raw = rawFixture
      val before = raw.countPalette
      raw.setColor(3, INIT_COLOR)
      val after = raw.countPalette
      before shouldEqual after
      raw.color(3) shouldEqual INIT_COLOR
    }

    "drawImage" when {

      def testEqualityAllPixel
      (
          prefix: String,
          x: Int = 0,
          y: Int = 0
      ): Boolean = {
        val l = ext.PNG.read(s"testdata/drawtest/${prefix}1.png")
        val r = ext.PNG.read(s"testdata/drawtest/${prefix}2.png")
        val e = ext.PNG.read(s"testdata/drawtest/${prefix}Expect.png")

        val raw = l.drawImageIndexColor(r, x, y)
        val buf = raw.toBufferedImage(l.getWidth)
        ImageResult(s"${prefix}Dest", buf).write("temp/")

        val res = ext.PNG.read(s"temp/${prefix}Dest.png")
        res.forallPixel(e)(_ => true){
          // ignore alpha (palette index 0 issue)
          (l, r) => l.rgb === r.rgb
        }
      }

      "draw simple image" in {
        testEqualityAllPixel("t") shouldBe true
        testEqualityAllPixel("b") shouldBe true
        testEqualityAllPixel("c", 15, 15) shouldBe true
      }
    }

  }



}
