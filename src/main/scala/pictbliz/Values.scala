package pictbliz

import java.net.URI

object Values {

  trait Value {
    def render(params: Params): ImagePart
  }

  case class Text(str: String) extends Value {

    def render(params: Params): ImagePart = {
      val strgraphics = StrGraphics.build(str, params)
      val res = strgraphics.processImage()
      strgraphics.dispose()
      ImagePart(Images.findBeginPoint(params, res.getWidth, res.getHeight), res)
    }
  }

  case class Icon(uri: URI) extends Value {
    def render(params: Params): ImagePart = ???
  }

  case class FaceGraphic(uri: URI, no: Int, transparent: Boolean) extends Value {
    def render(params: Params): ImagePart = ???
  }

  case class CharaGraphic(uri: URI, prop: CharaProperty, transparent: Boolean = true) extends Value {
    def render(params: Params): ImagePart = ???
  }
  case class CharaProperty(no: Int, dir: Int, act: Int)

  case class BattleGraphic(uri: URI, no: Int, transparent: Boolean = true) extends Value {
    def render(params: Params): ImagePart = ???
  }



  // etc...
}

abstract class Renderer[T] {


}