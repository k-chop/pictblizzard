package pictbliz
package extractor

class Tkool2kDBExtractorSpec extends UnitSpec {
  import Tkool2kDBExtractor._

  "Tkool2kDBExtractor" should {

    "do ber compression" in {
      ber2int(int2ber(2)) shouldEqual 2
      ber2int(int2ber(-1)) shouldEqual -1
      ber2int(int2ber(2356782)) shouldEqual 2356782
      ber2int(int2ber(-999999)) shouldEqual -999999
      ber2int(int2ber(999999)) shouldEqual 999999
    }
  }
}
