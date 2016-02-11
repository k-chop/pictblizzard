package pictbliz.extractor


sealed trait EntryDetail[T] {
  val default: T
}
case class EntryDetailInt(default: Int) extends EntryDetail[Int]
case class EntryDetailString(default: String) extends EntryDetail[String]
case class EntryDetailMappedInt(default: Int, map: Map[Int, String]) extends EntryDetail[Int]
case class EntryDetailReference(default: Int, newArgs: String, process: Int => String) extends EntryDetail[Int]
case class EntryDetailFlag(default: Boolean) extends EntryDetail[Boolean]
case class EntryDetailArray1d(default: String) extends EntryDetail[String]
case class EntryDetailArray2d(default: String) extends EntryDetail[String]

sealed trait AccessResult {
  def result: String
}
case class Result(str: String) extends AccessResult {
  def result = str
}
case class Reference(newArgs: Seq[String]) extends AccessResult {
  def result = newArgs.mkString(" ")
}

trait Accessor {
  // TODO: Load from mapping files!
  // mapping that keyword to index.
  val keymap: Map[String, Int]
  // mapping that index to EntryDetail.
  val indexmap: Map[Int, EntryDetail[_]]

  // alias
  def str(default: String) = EntryDetailString(default)
  def int(default: Int) = EntryDetailInt(default)
  def mint(default: Int, map: Map[Int, String]) = EntryDetailMappedInt(default, map)
  def ref(default: Int, args: String, process: Int => String) = EntryDetailReference(default, args, process)
  def flag(default: Boolean) = EntryDetailFlag(default)
  def arr1() = EntryDetailArray1d("")
}

class SkillAccessor(src: DBArray2) extends Accessor {

  val keymap: Map[String, Int] = Map(
    "名前"            -> 0x01,
    "説明"            -> 0x02,
    "使用時メッセージ1" -> 0x03,
    "使用時メッセージ2" -> 0x04,
    "失敗時メッセージ"  -> 0x07,
    "基本効果量"       -> 0x18
  )

  val indexmap: Map[Int, EntryDetail[_]] = Map(
    0x01 -> str(""),
    0x02 -> str(""),
    0x03 -> str(""),
    0x04 -> str(""),
    0x07 -> ref(0, "vocabulary #{ref}", i => s"${0x18 + i}"),
    0x18 -> int(0)
  )

  def get(index: Int, index2: Int, args: String*): AccessResult = {
    val ed = indexmap(index2)
    val res = src(index).asArray1At(index2)

    ed match {
      case EntryDetailString(default) =>
        val r = res.opt.asString().getOrElse(default)
        Result(r)
      case EntryDetailInt(default) =>
        val r = res.opt.asInt().getOrElse(default).toString
        Result(r)
      case EntryDetailReference(default, newArg, process) =>
        val i = res.opt.asInt().getOrElse(default)
        val n = newArg.replace("#{ref}", process(i))
        Reference(n.split(" "))
      case EntryDetailArray1d(_) =>
        getChilds(index2, res.asArray1(), args.head.toInt)
      case _ =>
        val r = res.opt.asString().getOrElse("")
        Result(r)
    }
  }

  def getChilds(idx: Int, arr: DBArray1, childIdx: Int): AccessResult = {
    val qr = arr.at(childIdx)
    idx match {
      case 0x2A =>
      case 0x2C =>
      case 0x10 =>
    }
  }

  def get(index: Int, keyword: String, args: String*): AccessResult =
    get(index, keymap(keyword), args: _*)

}

class VocabularyAccessor(src: DBArray1) extends Accessor {

  val keymap: Map[String, Int] = Map(
    "特殊技能失敗A" -> 0x18,
    "特殊技能失敗B" -> 0x19,
    "特殊技能失敗C" -> 0x1A,
    "物理攻撃回避"  -> 0x1B,
    "ニューゲーム"   -> 0x72,
    "コンティニュー" -> 0x73,
    "シャットダウン" -> 0x75
  )
  val indexmap: Map[Int, EntryDetail[_]] = Map(
    0x18 -> str(""),
    0x19 -> str(""),
    0x1A -> str(""),
    0x1B -> str(""),
    0x72 -> str(""),
    0x73 -> str(""),
    0x75 -> str("")
  )

  def get(index: Int): AccessResult = {
    val default = indexmap(index) match {
      case EntryDetailString(d) => d
      case _ => ""
    }
    val s = src.at(index).opt.asString().getOrElse(default)
    Result(s)
  }

  def get(keyword: String): AccessResult = {
    if (keyword.forall(_.isDigit)) // this is bad...
      get(keyword.toInt)
    else
      get(keymap(keyword))
  }

}