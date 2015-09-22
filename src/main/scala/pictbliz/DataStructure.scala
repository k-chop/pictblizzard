package pictbliz

import java.net.URI

import scala.reflect.ClassTag

object DataStructure {

  // Key
  type Id = String

  // DrawableImage
  class ResultImage
  // ResultImage
  case class ImagePart(pos: (Int, Int), image: ResultImage)

  case class WholeLayout(
    size: (Int, Int),
    parts: Seq[(Id, PartLayout)]
  )

  // AValue
  trait Value {
    def render(params: Params): ImagePart
  }
  case class Text(str: String) extends Value {
    def render(params: Params): ImagePart = ???
  }
  case class Icon(uri: URI) extends Value {
    def render(params: Params): ImagePart = ???
  }
  // etc...

  // AreaUnit
  case class PartLayout(params: Params) {
    def render(value: Value): ImagePart = value.render(params)
  }

  // Drawer
  case class Generator(whole: WholeLayout) {

    // Drawer#draw
    def genImage(values: Map[Id, Value]): ResultImage = {
      val imageparts = whole.parts.map {
        case (key, layout) =>
          layout.render(values(key))
      }
      //imageparts.fold from BlankImage
      new ResultImage
    }
  }

  // Attributes (AttrMap)
  case class Params(
    //
    border: String,
    //
    onCenter: String,
    //
    padding: String,
    //
    align: String,
    //
    point: String,
    //
    rect: String,
    //
    window: String,
    //
    background: String,
    //
    tile: String,
    //
    frontColor: String,
    //
    hemming: String,
    //
    font: String,
    //
    interval: String
  )

  def merge(p1: Params, p2: Params): Params = ???

}
