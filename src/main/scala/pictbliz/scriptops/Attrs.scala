package pictbliz.scriptops


import java.net.URI
import pictbliz.UColor

object Values {

}

object Attrs {
  // aliases
  type Key = Symbol
  //type AttrMap = Map[Key, Attr]
  type ValueMap = Map[Key, AValue]
  type ExValueMap = Map[String, AnyValue]

  trait Attr

  trait AnyValue

  sealed trait AValue extends AnyValue with Drawable

  case class Str(s: String) extends AValue

  case class Icon(uri: URI) extends AValue

  case class Number(i: Int) extends AValue

  case class FaceGraphic(uri: URI, no: Int, transparent: Boolean) extends AValue

  case class CharaGraphic(uri: URI, prop: CharaProperty, transparent: Boolean = true) extends AValue
  case class CharaProperty(no: Int, dir: Int, act: Int)

  case class BattleGraphic(uri: URI, no: Int, transparent: Boolean = true) extends AValue

  case object NullValue extends AValue with ExValue

  sealed trait ExValue extends AnyValue

  case class ExStr(s: String) extends ExValue

  case class ExIcon(s: String, zerofillDigit: Int = 0) extends ExValue

  case class ExRange(seq: Array[Int]) extends ExValue

  case class ExCSV(csvpath: String, column: Int) extends ExValue

  //case class ExDatabase() extends ExValue

  object LayoutUnit {
    def empty() = LayoutUnit.apply(new AttrMap, AreaMap.empty())
  }
  case class LayoutUnit(env: AttrMap, areamap: AreaMap)
  case class AreaUnit(attrmap: AttrMap) {


  }

  // attr_name
  // class definition

  // point
  case class APoint(x: Int, y: Int) extends Attr

  // rect
  case class ARect(x: Int, y: Int, w: Int, h: Int) extends Attr

  // auto_expand
  case object AAutoExpand extends Attr

  // on_center
  case object AOnCenter extends Attr

  // font
  case class AFont(name: String, style: Symbol, size: Int, lang: FontLang = Ja) extends Attr

  // hemming
  case class AHemming(color: UColor, size: Int) extends Attr

  // interval
  case class AInterval(xparam: Int, yparam: Int) extends Attr

  // padding
  case class APadding(xparam: Int, yparam: Int) extends Attr

  // align
  case class AAlign(xparam: Symbol, yparam: Symbol) extends Attr

  // border
  case object ABorder extends Attr

  // nil
  case object ANil extends Attr

  // front_color or back_color
  case class ASystemGraphics(path: String) extends Attr

  // front_color or back_color
  case class ASingleColors(colors: Array[String]) extends Attr

  // size
  case class ASize(w: Int, h: Int) extends Attr

  sealed trait Drawable

  case class ATile() extends Attr with Drawable

  case class ABackground() extends Attr with Drawable

  case class AWindow(systemGraphicsPath: String) extends Attr with Drawable


  sealed trait FontLang

  case object Ja extends FontLang

  case object En extends FontLang


  sealed trait Context

  class LexicalContext extends Context

  object NullContext extends Context

}
