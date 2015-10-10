package pictbliz

import Params._

import scalaz.syntax.semigroup._

class ImagesSpec extends UnitSpec {

  "Images" should {
    "find begin point collectly with method findBeginPoint" in {
      val a = Params make point(0, 1)
      val b = Params make rect(7, 9, 9, 11)
      val c = Params make (rect(3, 5, 1, 1) |+| onCenter)
      val d = Params make (point(160, 120) |+| onCenter)

      Images.findBeginPoint(a, 0, 0) should be ((0, 1))
      Images.findBeginPoint(b, 0, 0) should be ((7, 9))
      Images.findBeginPoint(c, 1, 1) should be ((3, 5))
      Images.findBeginPoint(d, 320, 240) should be ((0, 0))
    }
  }

}
