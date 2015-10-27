package pictbliz
package enrich

class PackedColorIntSpec extends UnitSpec {

  "PackedColorInt" should {

    "extract 0xAARRGGBB each byte value from Int" in {
      import enrich.packedcolor._

      val p1 = 0xFFAABB88
      val (a1,r1,g1,b1) = p1.argb
      a1 should equal (0xFF)
      r1 should equal (0xAA)
      g1 should equal (0xBB)
      b1 should equal (0x88)

      val p2 = 0x0123153F
      val (a2,r2,g2,b2) = p2.argb
      a2 should equal (0x01)
      r2 should equal (0x23)
      g2 should equal (0x15)
      b2 should equal (0x3F)

      val p3 = 0xFFFFFFFF
      val (a3,r3,g3,b3) = p3.argb
      a3 should equal (0xFF)
      r3 should equal (0xFF)
      g3 should equal (0xFF)
      b3 should equal (0xFF)

      val p4 = 0x0
      val (a4,r4,g4,b4) = p4.argb
      a4 should equal (0x00)
      r4 should equal (0x00)
      g4 should equal (0x00)
      b4 should equal (0x00)

      val p5 = -2
      val (a5,r5,g5,b5) = p5.argb
      a5 should equal (0xFF)
      r5 should equal (0xFF)
      g5 should equal (0xFF)
      b5 should equal (0xFE)
    }
  }

}
