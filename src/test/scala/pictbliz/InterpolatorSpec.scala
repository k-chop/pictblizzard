package pictbliz

import Values._

class InterpolatorSpec extends UnitSpec {

  final val CREText = Text("Circular Reference Error Occurred.")

  "Interpolator" should {

    "do nothing with normal string" in {
      val res = new Interpolator(
        Map("test" -> Text("test"))).interpolate(0)

      res("test") should equal (Text("test"))
      res("id") should equal (Text("0"))
    }

    "interpolate correctly single #{} string" in {
      val res = new Interpolator(
        Map("test" -> Text("test"),
            "inp" -> Text("interpolated: #{test}"))).interpolate(0)

      res("inp") should equal (Text("interpolated: test"))
    }

    "interpolate correctly twice nested #{} string" in {
      val res = new Interpolator(
        Map("test" -> Text("test"),
            "inp1" -> Text("interpolated: #{test}"),
            "inp2" -> Text("interpolated twice: #{inp1}"))).interpolate(0)

      res("inp2") should equal (Text("interpolated twice: interpolated: test"))
    }

    "detect circular reference error" in {
      val res = new Interpolator(
        Map("A" -> Text("#{B}"),
            "B" -> Text("#{A}"))).interpolate(0)

      res("A") should equal (CREText)
      res("B") should equal (CREText)
    }

    "interpolate correctly multiple #{} string" in {
      val res = new Interpolator(
        Map("A" -> Text("the Answer to The Ultimate Question of Life, the Universe, and Everything"),
            "B" -> Text("42"),
            "C" -> Text("#{A} is #{B}."))).interpolate(0)

      res("C") should equal (Text("the Answer to The Ultimate Question of Life, the Universe, and Everything is 42."))
    }

    "detect deep circular reference error" in {
      val begin = 1
      val end = 101  // 1001 is OK, but it takes time to finish test. 10001 cause StackOverflow :(
      val map = (begin to end-1) zip (begin+1 to end) map { case (k, v) =>
        k.toString -> Text(s"#{${ if (v == end) 1 else v}}")
      } toMap
      val res = new Interpolator(map).interpolate(0)
      res("1") should equal (CREText)
    }

  }

}
