package com.github.chuwb.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import scala.collection.mutable.Stack

class TestSpec extends WordSpec with ShouldMatchers {

  "Ops" should {
    
    import ScriptOps._
    val lay = LayoutUnit( (320, 60), Map(
      "name"->AreaUnit(Str, Map("rect"->Attr(Seq("5","10","200","13")))),
      "icon"->AreaUnit(Icon, Map("rect"->Attr(Seq("0","0","32","32")))),
      "desc"->AreaUnit(Str, Map("rect"      -> Attr(Seq("8","25","280","13")),
                                "x-interval"-> Attr(Seq("5")))),
      "cost"->AreaUnit(Str, Map("rect"->Attr(Seq("280","45","30","15")),
                                "font"->Attr(Seq("Verdana", "plain", "10"))))
    ))
    
    println(lay)

    val testunit = FileUnit(
      Map("name"->"エターナルドリルクラッシュ",
          "icon"->"icon/icon1.png",
          "desc"->"ひたすらドリルで突き刺す。相手は死ぬ",
          "cost"->"42"),
      lay
    )

    (new Drawer(testunit, NullContext)).draw().write("temp/TestSpec.png")

    "a" in { val s = 1
      s should be (s) }
  }

  
  // "stack" should {
  //   val stack = new Stack[Int]
  //   "be empty" in {
  //     stack should be ('empty)
  //   }
  //   "complain when popped" in {
  //     evaluating { stack.pop() } should produce [NoSuchElementException]
  //   }
  // }

}
