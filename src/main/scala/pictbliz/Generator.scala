package pictbliz

import Layouts._

class Generator(layout: WholeLayout) {

  def genImage(values: Map[Layouts.Id, Values.Value]): ImageResult = {
    import scalaz.syntax.semigroup._

    val (width, height) = layout.size

    val partImages = layout.parts.map { case (id, part) =>
      values.get(id).fold(Images.emptyPart)(_.render(part.params))
    }

    partImages.foldLeft(Images.blank(width, height))(_ |+| _)
        .result(values)
  }
}