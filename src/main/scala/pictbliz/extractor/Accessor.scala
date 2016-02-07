package pictbliz.extractor


sealed trait DataType
case object DInt extends DataType
case object DStr extends DataType
case object DFlag extends DataType
case object DArray1d extends DataType
case object DArray2d extends DataType

private[extractor] case class EntryDetail(index: Int, dataType: DataType, default: String)

trait Accessor {
  // mapping that keyword to EntryDetail. TODO: Load from mapping files!
  val map: Map[String, EntryDetail]

  // alias
  def e(index: Int, dataType: DataType, default: String) = EntryDetail(index, dataType, default)
}

class SkillAccessor(src: DBArray2) extends Accessor {

  val map: Map[String, EntryDetail] = Map(
    "名前"      -> e(0x01, DStr, ""),
    "基本効果量" -> e(0x18, DInt, "0")
  )

  def get(index: Int, keyword: String, args: Array[String] = Array.empty[String]): String = {
    val ed = map(keyword)
    val res = src(index).asArray1At(ed.index)

    val r = ed.dataType match {
      case DStr => res.opt.asString()
      case DInt => res.opt.asInt().map(_.toString)
      case _ => res.opt.asString()
    }
    r.getOrElse(ed.default)
  }
}

class VocabularyAccessor(src: DBArray1) extends Accessor {

  val map: Map[String, EntryDetail] = Map(
    "ニューゲーム"   -> e(0x72, DStr, ""),
    "コンティニュー" -> e(0x73, DStr, ""),
    "シャットダウン" -> e(0x75, DStr, "")
  )

  def get(keyword: String): String =
    src.at(map(keyword).index).opt.asString().getOrElse(map(keyword).default)

}