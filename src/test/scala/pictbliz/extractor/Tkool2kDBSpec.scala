package pictbliz
package extractor


class Tkool2kDBSpec extends UnitSpec {

  "Tkool2kDB" should {

    "build from RPG_RT.ldb" in {
      import Tkool2kDBExtractor._

      val db = Tkool2kDB.fromFile("testdata/no-v/RPG_RT.ldb")
      val buf = db.bytes

      db.heroes.position shouldEqual 15
      db.seek(db.vocabulary)
      nextBer(buf) shouldEqual 1
      nextStr(buf) shouldEqual "が出現！"

    }

  }
}
