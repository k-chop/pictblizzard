package pictbliz

import RawIndexColorImage._

class RawIndexColorImageSpec extends UnitSpec {

  "RawIndexColorImage" should {

    def rawFixture = RawIndexColorImage.fromSize(5, 0xff)

    "create empty RawIndexColorImage from fromSize" in {
      val raw = rawFixture
      raw.countEmptyPalette shouldEqual (0xff - 1)
      raw.palette(0) shouldBe INIT_COLOR
      all (raw.pixels) should equal (0)
    }

    "set color with extending palette" in {
      val raw = rawFixture
      val nc = 0xffeeeeee
      raw.setColor(0, nc)
      raw.countEmptyPalette shouldEqual (0xff - 2)
      raw.findPalette(nc).value shouldEqual 1
      raw.palette should contain (nc)
    }

    "set color without extending palette" in {
      val raw = rawFixture
      val before = raw.countEmptyPalette
      raw.setColor(3, INIT_COLOR)
      val after = raw.countEmptyPalette
      before shouldEqual after
      raw.color(3) shouldEqual INIT_COLOR
    }
  }

}
