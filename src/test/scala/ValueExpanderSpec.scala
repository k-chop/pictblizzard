package com.github.chuwb.pictbliz.test

import com.github.chuwb.pictbliz._
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import com.github.chuwb.pictbliz.ScriptOps.{ExCSV, ExRange, Str, ExStr}

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

        )
      )
    }

  }
}
