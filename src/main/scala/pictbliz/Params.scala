package pictbliz

import pictbliz.Params._

import scalaz.Endo
import scalaz.syntax.std.option._
import scalaz.syntax.semigroup._

object Params extends ParamSetters {

  case class Point(x: Int, y: Int)

  case class Rect(x: Int, y: Int, w: Int, h: Int)

  case class Font(name: String, style: Symbol, size: Int)

  case class Hemming(color: UColor, size: Int)

  case class Interval(x: Int, y: Int)

  case class Padding(x: Int, y: Int)

  object Align {
    trait Vertical
    trait Horizontal
    case object Center extends Vertical with Horizontal
    case object Left extends Horizontal
    case object Right extends Horizontal
    case object Top extends Vertical
    case object Bottom extends Vertical
  }

  case class Align(x: Align.Horizontal, y: Align.Vertical)

  //
  case class Size(w: Int, h: Int)

  case class Tile(path: String)

  case class Background(path: String)

  case class Window(systemGraphicsPath: String) {
    // hmm....
    def toValue: Values.Window = Values.Window(systemGraphicsPath)
  }

  //
  type ParamSet = Endo[Params]

}

sealed abstract class ParamSetters {

  // setter
  def point(x: Int, y: Int): ParamSet =
    Endo(_.copy(point = Point(x, y).some))
  def rect(x: Int, y: Int, w: Int, h: Int): ParamSet =
    Endo(_.copy(rect = Rect(x, y, w, h).some))
  def font(name: String, style: Symbol, size: Int): ParamSet =
    Endo(_.copy(font = Font(name, style, size).some))
  def hemming(color: UColor, size: Int): ParamSet =
    Endo(_.copy(hemming = Hemming(color, size).some))
  def interval(x: Int, y: Int): ParamSet =
    Endo(_.copy(interval = Interval(x, y).some))
  def padding(x: Int, y: Int): ParamSet =
    Endo(_.copy(padding = Padding(x, y).some))
  def align(x: Align.Horizontal, y: Align.Vertical): ParamSet =
    Endo(_.copy(align = Align(x, y)))
  def window(systemGraphicsPath: String): ParamSet =
    Endo(_.copy(window = Window(systemGraphicsPath).some))
  def frontColor(systemGraphicsPath: String): ParamSet =
    Endo(_.copy(frontColor = SystemGraphics.make(systemGraphicsPath)))
  def border: ParamSet =
    Endo(_.copy(border = true))
  def onCenter: ParamSet =
    Endo(_.copy(onCenter = true))
  def autoExpand: ParamSet =
    Endo(_.copy(autoExpand = true))

  // util
  def defaultFont: ParamSet = font("MS Gothic", 'plain, 12)
  def message: ParamSet = interval(0, 3) |+| padding(8, 10)
  def stdStyle: ParamSet = defaultFont |+| message
  def autoExpandSizeWith(ip: ImagePart): ParamSet = Endo( p =>
    if (p.autoExpand) {
      val (mx, my) = p.rect.collect{ case Rect(x, y, _, _) => (x, y) }.getOrElse((0, 0))
      p.copy(rect = Rect(mx, my, ip.image.getWidth, ip.image.getHeight).some)
    } else p
  )

  def make(ps: ParamSet): Params = ps apply Params()

}

// Parameters for Layout
case class Params(
    //
    border: Boolean = false,
    //
    onCenter: Boolean = false,
    //
    autoExpand: Boolean = false,
    //
    padding: Option[Padding] = None,
    //
    align: Align = Align(Align.Left, Align.Top),
    //
    point: Option[Point] = None,
    //
    rect: Option[Rect] = None,
    //
    window: Option[Window] = None,
    //
    background: Option[Background] = None,
    //
    tile: Option[Tile] = None,
    //
    frontColor: Texturable = SystemGraphics.default,
    //
    hemming: Option[Hemming] = None,
    //
    font: Option[Font] = None,
    //
    interval: Option[Interval] = None
    ) {

  def merge: ParamSet = Endo(identity)

}

