package com.github.chuwb.pictbliz

import scala.collection.mutable

class AttributedText(val raw: String) {

  val (ranges, string) = parse(raw)
  def iter = ranges.iterator

  def parse(source: String): (List[AttributeRange], String) = {
    // バックスラッシュから始まる特殊文字を元の文字列から消去し、
    // AttributeRangeに詰めるだけの簡単なお仕事。
    val acc = mutable.ListBuffer.empty[AttributeRange]

    sys.error("undefined")
    //(acc.toList, source)
  }
  
}
