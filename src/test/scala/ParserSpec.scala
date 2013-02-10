package com.github.whelmaze.pictbliz.test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import com.github.whelmaze.pictbliz.scriptops.Attrs._
import com.github.whelmaze.pictbliz.scriptops._
import com.github.whelmaze.pictbliz.Resource

class ParserSpec extends WordSpec with ShouldMatchers {

  "Parser" should {

    "parse script file" in {
      val a = io.Source.fromFile(Resource.file("scriptTest.txt"))(io.Codec.UTF8)
      Parser.parse(a.mkString)
      true
    }

  }
}
