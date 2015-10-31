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
        testDraw("t", 0, 0) shouldBe true
        testDraw("b", 0, 0) shouldBe true
        testDraw("c", 15, 15) shouldBe true
      }
    }

    "trimming" when {

      "equal trimmed whole image" in {
        //testTrim("a", 0, 0, ) shouldBe true
      }
    }

    "synthesis" when {

      "synthesize index-color image" in {

        testSynth("m") shouldBe true
      }
    }

  }



}
