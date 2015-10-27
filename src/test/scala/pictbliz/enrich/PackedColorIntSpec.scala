package pictbliz
package enrich

import enrich.packedcolor._

class PackedColorIntSpec extends UnitSpec {

  "PackedColorInt" should {

    "extract 0xAARRGGBB each byte value from Int" in {

      0xFFAABB88.argb shouldEqual ((0xFF, 0xAA, 0xBB, 0x88))

      0x0123153F.argb shouldEqual ((0x01, 0x23, 0x15, 0x3F))

      0xFFFFFFFF.argb shouldEqual ((0xFF, 0xFF, 0xFF, 0xFF))

      0.argb shouldEqual ((0x00, 0x00, 0x00, 0x00))

      (-2).argb shouldEqual ((0xFF, 0xFF, 0xFF, 0xFE))
    }

    "convert to HSV color space" in {

      rgb(105,133,232).hsv shouldEqual ((226, 139, 232))

    }

    "convert to RGB from RGB" in {

      rgb(0, 0, 0).rgb shouldEqual ((0, 0, 0))
      rgb(0, 0, 0).argb shouldEqual ((0xff, 0, 0, 0))
      rgb(255, 255, 255).rgb shouldEqual ((255, 255, 255))
    }

  }

}
