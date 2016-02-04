package pictbliz
package extractor

class Tkool2kDBSpec extends UnitSpec {

  "Tkool2kDB" should {

    "build from RPG_RT.ldb" in {

      val db = Tkool2kDB.fromFile("testdata/no-v/RPG_RT.ldb")

      db.heroes.position shouldEqual 15
      db.vocabulary.asString(0x01).value shouldBe "が出現！"

      db.vocabulary.indices.size shouldEqual 120
      db.vocabulary.asString(0x01).value shouldEqual "が出現！"
      db.vocabulary.asString(0x97).value shouldEqual "終了してよろしいですか？"
      db.vocabulary.asString(0x91) shouldBe empty // 0x91 is empty entry

      db.heroes.indices.size shouldEqual 8

      db.skills.indices.size shouldEqual 128

      val skillDetail = db.skills.asArray1(127)
      skillDetail.asString(0x02).value shouldEqual "敵全体に無属性のダメージを与える。"

    }

  }
}
