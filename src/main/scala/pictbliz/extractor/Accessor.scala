package pictbliz.extractor


trait Accessor {
  // mapping that keyword to index
  val map: Map[String, Int]
}

class VocabularyAccessor(src: DBArray1) extends Accessor {

  // TODO: Load from mapping files!
  val map: Map[String, Int] = Map(
    "ニューゲーム" -> 0x72,
    "コンティニュー" -> 0x73,
    "シャットダウン" -> 0x75
  )

  def get(keyword: String): String = src.at(map(keyword)).asString()

}