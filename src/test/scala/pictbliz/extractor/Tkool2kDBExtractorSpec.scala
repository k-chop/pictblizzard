package pictbliz
package extractor

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
  }
}
