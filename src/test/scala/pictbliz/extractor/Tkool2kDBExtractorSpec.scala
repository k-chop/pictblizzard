package pictbliz
package extractor

import java.nio.ByteBuffer

class Tkool2kDBExtractorSpec extends UnitSpec {
  import Tkool2kDBExtractor._
  import scala.collection.breakOut

  object ByteArray {
    def apply(self: Int*): Array[Byte] = self.map(_.toByte)(breakOut)
  }

  "Tkool2kDBExtractor" should {

    "do ber compression" in {
      ber2int(int2ber(0)) shouldEqual 0
      ber2int(int2ber(2)) shouldEqual 2
      ber2int(int2ber(-1)) shouldEqual -1
      ber2int(int2ber(2356782)) shouldEqual 2356782
      ber2int(int2ber(-999999)) shouldEqual -999999
      ber2int(int2ber(999999)) shouldEqual 999999

      int2ber(ber2int(ByteArray(0))) shouldEqual ByteArray(0)
      int2ber(ber2int(ByteArray(0x7f))) shouldEqual ByteArray(0x7f)
      int2ber(ber2int(ByteArray(0xff, 0x7f))) shouldEqual ByteArray(0xff, 0x7f)
      int2ber(ber2int(ByteArray(0xff, 0xae, 0x7f))) shouldEqual ByteArray(0xff, 0xae, 0x7f)
      int2ber(ber2int(ByteArray(0xff, 0xff, 0xff, 0x7f))) shouldEqual ByteArray(0xff, 0xff, 0xff, 0x7f)
    }

    "read DB file as ByteBuffer" in {
      import Tkool2kDB._

      val buf = Tkool2kDB.asByteBuffer("testdata/no-v/RPG_RT.ldb")
      buf.position() shouldEqual 0
      buf.limit() shouldEqual 190974
      buf.capacity() shouldEqual 190974

      val ba = Array.ofDim[Byte](0x0C)
      buf.get(ba, 0, 0x0c)
      ba shouldEqual ByteArray(0x0B, 0x4c, 0x63, 0x66, 0x44, 0x61, 0x74, 0x61, 0x42, 0x61, 0x73, 0x65)

      nextBer(buf) shouldEqual 0x0B
      nextBer(buf) shouldEqual 5941
      buf.forward(5941)
      nextBer(buf) shouldEqual 0x0C
      nextBer(buf) shouldEqual 14146
      buf.forward(14146)
      nextBer(buf) shouldEqual 0x0D
    }

    "nextBer" in {

      val ba = ByteBuffer.wrap(ByteArray(0xff, 0xff, 0x00, 0xff, 0xfa, 0x1c, 0xee, 0x3f))
      nextBer(ba) shouldEqual 2097024
      ba.position shouldEqual 3
      nextBer(ba) shouldEqual 2096412
      ba.position shouldEqual 6
      nextBer(ba) shouldEqual 14143
      ba.position shouldEqual ba.limit
    }
  }
}
