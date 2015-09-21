package pictbliz
package test

import scriptops.implicits.string2URI
import scala.language.postfixOps

import scriptops.Attrs._

class ValueExpanderSpec extends UnitSpec {

  "ValueExpander" should {
    "expand 1 correctly" in {
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(1 to 1 toArray),
        "no1" -> ExStr("${id}:asd${ok}"),
        "ok" -> ExStr("ok${bokujou}"),
        "bokujou" -> Str("bokujou")
      ))

      val res = vmap.expand()
      val ans = Array(Map(
        'id -> Str("1"),
        'no1 -> Str("1:asdokbokujou"),
        'ok -> Str("okbokujou"),
        'bokujou -> Str("bokujou")
      ))
      res(0) shouldBe ans(0)
    }

    "expand 2 correctly" in {
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(1 to 2 toArray),
        "no1" -> ExStr("${id}:asd${ok}"),
        "ok" -> ExStr("ok${bokujou}"),
        "bokujou" -> Str("bokujou")
      ))

      val res = vmap.expand()
      val ans = Array(
        Map(
          'id -> Str("1"),
          'no1 -> Str("1:asdokbokujou"),
          'ok -> Str("okbokujou"),
          'bokujou -> Str("bokujou")
        ),
        Map(
          'id -> Str("2"),
          'no1 -> Str("2:asdokbokujou"),
          'ok -> Str("okbokujou"),
          'bokujou -> Str("bokujou")
        )
      )
      res(0) shouldBe ans(0)
      res(1) shouldBe ans(1)
    }

    "expand csv correctly" in {
      val path = "lookuptest.csv"
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(0 to 1 toArray),
        "name" -> ExCSV(path, 1),
        "sex" -> ExCSV(path, 2),
        "etc" -> ExCSV(path, 3),
        "all" -> ExStr("${id}:${name}(${sex}), ${etc}")
      ))
      val res = vmap.expand()
      val ans = Array(
        Map(
          'id -> Str("0"),
          'name -> Str("tarou"),
          'sex -> Str("otoko"),
          'etc -> Str("ikemen"),
          'all -> Str("0:tarou(otoko), ikemen")
        ),
        Map(
          'id -> Str("1"),
          'name -> Str("jirou"),
          'sex -> Str("otoko"),
          'etc -> Str("neet"),
          'all -> Str("1:jirou(otoko), neet")
        )
      )
      res(0) shouldBe ans(0)
      res(1) shouldBe ans(1)
    }

    "return empty Array with valuemap has no key('id')" in {
      val vmap = new ValueExpander(Map(
        "name" -> ExStr("${test}"),
        "test" -> ExStr("${hoge}"),
        "hoge" -> ExStr("fuga")
      ))
      val res = vmap.expand()
      val ans = Array()
      ans should have length 0
      res should have length 0
    }

    "expand ExStr(no more expand)" in {
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(1 to 1 toArray),
        "name" -> ExStr("${test}"),
        "test" -> ExStr("${hoge}"),
        "hoge" -> ExStr("fuga")
      ))
      val res = vmap.expand()
      val ans = Array(
        Map(
          'id -> Str("1"),
          'name -> Str("fuga"),
          'test -> Str("fuga"),
          'hoge -> Str("fuga")
        )
      )
      res(0) shouldBe ans(0)
    }

    "expand ExIcon(no zerofill)" in {
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(1 to 2 toArray),
        "icon" -> ExIcon("icon/icon${id}.png")
      ))
      val res = vmap.expand()
      val ans = Array(
        Map(
          'id -> Str("1"),
          'icon -> Icon("icon/icon1.png")
        ),
        Map(
          'id -> Str("2"),
          'icon -> Icon("icon/icon2.png")
        )
      )
      res(0) shouldBe ans(0)
      res(1) shouldBe ans(1)
    }

    "expand ExIcon(with zerofill)" in {
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(1 to 1 toArray),
        "icon1" -> ExIcon("icon/icon${id}${ext}", zerofillDigit = 1),
        "icon2" -> ExIcon("icon/icon${id}${ext}", zerofillDigit = 2),
        "icon3" -> ExIcon("icon/icon${id}${ext}", zerofillDigit = 3),
        "ext" -> Str(".png")
      ))
      val res = vmap.expand()
      val ans = Array(
        Map(
          'id -> Str("1"),
          'icon1 -> Icon("icon/icon1.png"),
          'icon2 -> Icon("icon/icon01.png"),
          'icon3 -> Icon("icon/icon001.png"),
          'ext -> Str(".png")
        )
      )
      res(0) shouldBe ans(0)
    }

    "expand circular reference" in {
      val vmap = new ValueExpander(Map(
        "id" -> ExRange(1 to 2 toArray),
        "a" -> ExStr("${b}"),
        "b" -> ExStr("${a}")
      ))
      val res = vmap.expandAt(1)
      val ans = Array(
        Map(
          'id -> Str("1"),
          'a -> Str("Circular Reference Error"),
          'b -> Str("Circular Reference Error")
        )
      )
      res shouldBe ans(0)
    }

  }
}
