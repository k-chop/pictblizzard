package pictbliz

import scalaz.Endo

object Layouts {

  // type aliases

  type Id = String

  type VMap = Map[Layouts.Id, Values.Value]

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