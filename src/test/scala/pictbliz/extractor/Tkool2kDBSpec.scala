package pictbliz
package extractor


class Tkool2kDBSpec extends UnitSpec {

  "Tkool2kDB" should {

    "build from RPG_RT.ldb" in {

      val db = Tkool2kDB.fromFile("testdata/no-v/RPG_RT.ldb")

      db.heroes.position shouldEqual 15
      db.seek(db.vocabulary)
      db.nextBer() shouldEqual 1
      db.nextStr() shouldEqual "が出現！"

      val vocabIndices = db.makeIndices1(db.vocabulary)
      vocabIndices.size shouldEqual 120
      db.seek(vocabIndices(0x01))
      db.nextStr() shouldEqual "が出現！"
      db.seek(vocabIndices(0x97))
      db.nextStr() shouldEqual "終了してよろしいですか？"
      vocabIndices(0x91) shouldEqual -1 // 0x91 is empty entry

      val heroIndices = db.makeIndices2(db.heroes)
      heroIndices.size shouldEqual 8

      val skillIndices = db.makeIndices2(db.skills)
      skillIndices.size shouldEqual 128

      val skillDetailIndices = db.makeIndices1(skillIndices(127))
      db.seek(skillDetailIndices(0x02))
      db.nextStr() shouldEqual "敵全体に無属性のダメージを与える。"
    }

  }
}
