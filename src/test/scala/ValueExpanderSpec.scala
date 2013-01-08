package com.github.whelmaze.pictbliz.test

import com.github.whelmaze.pictbliz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import scriptops.implicits.string2URI
import scala.language.postfixOps

import scriptops.Attrs._

class ValueExpanderSpec extends WordSpec with ShouldMatchers {

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
      assert(isSame(res(0), ans(0)) === true)
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
      assert(isSame(res(0), ans(0)) === true)
      assert(isSame(res(1), ans(1)) === true)
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
      assert(isSame(res(0), ans(0)) === true)
      assert(isSame(res(1), ans(1)) === true)
    }

    "return empty Array with valuemap has no key('id')" in {
      val vmap = new ValueExpander(Map(
        "name" -> ExStr("${test}"),
        "test" -> ExStr("${hoge}"),
        "hoge" -> ExStr("fuga")
      ))
      val res = vmap.expand()
      val ans = Array()
      ans should have length (0)
      res should have length (0)
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
      assert(isSame(res(0), ans(0)) === true)
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
      assert(isSame(res(0), ans(0)) === true)
      assert(isSame(res(1), ans(1)) === true)
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
      assert(isSame(res(0), ans(0)) === true)
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
      assert(isSame(res, ans(0)) === true)
    }

  }
}
