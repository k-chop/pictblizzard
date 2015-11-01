package pictbliz

import RawIndexColorImage._

import enrich.all._

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


    "draw simple image" in {
      testDraw("t1", "t2", "tExpect", 0, 0)
      testDraw("b1", "b2", "bExpect", 0, 0)
      testDraw("c1", "c2", "cExpect", 15, 15)
    }


    "trim whole image" in {
      testTrim("trimWhole", "trimWhole", 0, 0, 16, 24)
    }
    "trim whole over image" in {
      testTrim("trimWhole", "trimWhole", 0, 0, Int.MaxValue, Int.MaxValue)
      testTrim("trimWhole", "trimFail", 0, 0, Int.MinValue, Int.MinValue)
    }
    "trim part image" in {
      testTrim("trimWhole", "trimmed2", 6, 14, 10, 8)
    }

    "synthesize index-color image" in {
      testSynth("m1", "m2")
      testSynth("n1", "n2", 0xffff00ff)
    }

  }

}
