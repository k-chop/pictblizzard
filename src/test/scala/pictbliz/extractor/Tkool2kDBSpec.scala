package pictbliz
package extractor


class Tkool2kDBSpec extends UnitSpec {

  "Tkool2kDB" should {

    "build from RPG_RT.ldb" in {

      val db = Tkool2kDB.fromFile("testdata/no-v/RPG_RT.ldb")

      db.heroes.position shouldEqual 13
    }

  }
}
