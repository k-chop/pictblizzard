package com.github.chuwb.pictbliz
package test

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class AttributedTextSpec extends WordSpec with ShouldMatchers {

  val AR = AttributeRange
  
  "AttributedText" should {

    "parse attributed text collectly" in {

      val ar = new AttributedText("""one\c[1]two\c[2]three\c[3]four""")
      ar.str should equal ("onetwothreefour")
      ar.ranges should have length (4)
      ar.ranges(2) should equal (AR(6,11,CtrColor(2)))

      val ar2 = new AttributedText("""one\c[1]two\c[2]three\c[3]four\c[2]""")
      ar2.str should equal ("onetwothreefour")
      ar2.ranges should have length (4)
      ar2.ranges(3) should equal (AR(11,15,CtrColor(3)))
      
      val ar3 = new AttributedText("""\c[0]\c[1]\c[0]\c[2]""")
      ar3.str should equal ("")
      ar3.ranges should have length (0)

      val ar4 = new AttributedText("""\c[1]test""")
      ar4.str should equal ("test")
      ar4.ranges should have length (1)
      ar4.ranges(0) should equal (AR(0,4,CtrColor(1)))
      
      val ar5 = new AttributedText("""test\c[1]""")
      ar5.str should equal ("test")
      ar5.ranges should have length (1)

      val ar6 = new AttributedText("""plain""")
      ar6.str should equal ("plain")
      ar6.ranges should have length (1)
      ar6.ranges(0) should equal (AR(0,5,CtrColor(0)))
      
    }
  }
}
