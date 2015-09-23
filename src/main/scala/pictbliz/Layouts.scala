package pictbliz

import scalaz.Endo

object Layouts {

  type Id = String

  case class WholeLayout(
      size: (Int, Int),
      parts: Seq[(Id, PartLayout)]
  )

  object PartLayout {

    final def ep(endo: Endo[Params]) = PartLayout(endo apply Params())
  }

  case class PartLayout(params: Params) {

    def render(value: Values.Value): ImagePart = value.render(params)
  }

}