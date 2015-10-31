package pictbliz

import RawIndexColorImage._

import enrich.all._
import pictbliz.ext.PNG

class RawIndexColorImageSpec extends UnitSpec with ImageSpec {

  "RawIndexColorImage" should {

    def rawFixture = RawIndexColorImage.fromArraySize(5, 0xff, 5)

    "create empty RawIndexColorImage from fromSize" in {
      val raw = rawFixture
      raw.countPalette shouldEqual 0
      raw.palette(0) shouldBe UNUSED
      all (raw.pixels) should equal (0)
    }

    "set color with extending palette" in {
      val raw = rawFixture
      val nc = 0xffeeeeee
      raw.setColor(0, nc)
      raw.countPalette shouldEqual 1
      raw.findPalette(nc).value shouldEqual 0
      raw.palette should contain (nc)
    }

    "set color without extending palette" in {
      val raw = rawFixture
      val before = raw.countPalette
      raw.setColor(3, UNUSED)
      val after = raw.countPalette
      before shouldEqual after
      raw.color(3) shouldEqual UNUSED
    }

    "drawImage" when {

      "draw simple image" in {
        implicit val testName = "draw"
        testEqualityAllPixelWithDraw("t", 0, 0) shouldBe true
        testEqualityAllPixelWithDraw("b", 0, 0) shouldBe true
        testEqualityAllPixelWithDraw("c", 15, 15) shouldBe true
      }
    }

    "trimming" when {

      "equal trimmed whole image" in {
        // :)
      }
    }

    "synthesis" when {

      "synthesize index-color image" in {
        def isNotAlpha(i: Int) = i.a != 0

        val mask = PNG.read("testdata/synth/mask.png")
        val grad = PNG.read("testdata/synth/grad.png")

        val res = ImageUtils.synthesisIndexColor(mask, grad)
        ImageResult("synth", res).write("temp/")

        val resb = PNG.read("temp/synth.png")

        // checking each pixel that is not transparent is (resb == grad)
        val p = resb.testAllPixel(grad)(index0AsAlpha = true)(isNotAlpha){
          (r, g) => r == g
        }
        assert(p)
      }
    }

  }



}
