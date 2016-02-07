package pictbliz
package extractor

class Tkool2kDBSpec extends UnitSpec {

  "Tkool2kDB" should {

    val db = Tkool2kDB.fromFile("testdata/no-v/RPG_RT.ldb")

    "build from RPG_RT.ldb" in {

      db.heroes.position shouldEqual 13
      db.vocabulary(0x01).opt.asString().value shouldBe "が出現！"

      db.vocabulary.indices.size shouldEqual 120
      db.vocabulary(0x01).opt.asString().value shouldEqual "が出現！"
      db.vocabulary(0x97).opt.asString().value shouldEqual "終了してよろしいですか？"
      db.vocabulary(0x91).opt.asString() shouldBe empty // 0x91 is empty entry

      db.heroes.indices.size shouldEqual 8

      db.skills.indices.size shouldEqual 128

      val skillDetail = db.skills(127).asArray1()
      skillDetail(0x02).opt.asString().value shouldEqual "敵全体に無属性のダメージを与える。"

      val hero1Details = db.heroes(0x01).asArray1()
      val learnSkillPerLevel = hero1Details(0x3f).asArray2()
      val atThree = learnSkillPerLevel(3).asArray1()
      atThree(0x01).asInt() shouldEqual 4
      atThree(0x02).asInt() shouldEqual 36

      val at = db.heroes(0x01).asArray1().at(0x3f).asArray2().at(3).asArray1()
      at(0x01).asInt() shouldEqual 4
      at(0x02).asInt() shouldEqual 36

      val at2 = db.heroes(0x01).asArray1At(0x3f).asArray2At(3).asArray1()
      at2(0x01).asInt() shouldEqual 4
      at2(0x02).asInt() shouldEqual 36
    }

    "access with keyword" in {

      val va = new VocabularyAccessor(db.vocabulary)
      va.get("ニューゲーム") shouldEqual "ニューゲーム"

      val sa = new SkillAccessor(db.skills)
      sa.get(1, "名前") shouldEqual "毒攻撃"
      sa.get(36, "基本効果量") shouldEqual "30"
    }

  }
}
