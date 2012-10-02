package com.github.whelmaze.pictbliz

import scala.collection.mutable

class AttributedText(val rawstr: String) {

  val (ranges, str) = parse(rawstr)
  def iter = ranges.iterator

  private[this] def parse(source: String): (List[AttributeRange], String) = {

    val acc = mutable.ListBuffer.empty[AttributeRange]
    val reseter = """\c[0]"""
    val sb = new StringBuffer()
    
    val addedsource = reseter + source + reseter

    @scala.annotation.tailrec
    def rec(trimmed: String, start: Int, diff: Int): Unit = {
      val re = """\\([a-z])\[(\d+)\]([^\\]*)\\""".r
      val matchResult = re.findFirstMatchIn(trimmed)
      if (matchResult.isDefined) {
        val m = matchResult.get
        val str = m.group(3)

        if (str.length != 0) {
          val comc = m.group(1).head
          val comidx = m.group(2).head - '0'  // ChartoInt

          val nextStart = m.start + str.length
          acc += AttributeRange(m.start, nextStart, ControlChar.build(comc, comidx))
          sb.append(str)
          rec(m.before + str + "\\" + m.after.toString, 0, 0)
        } else {
          rec(m.before + str + "\\" + m.after.toString, 0, 0)
        }
      }
    }

    rec(addedsource, 0, 0)
    (acc.toList, sb.toString)
  }
  
}
