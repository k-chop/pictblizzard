package com.github.whelmaze.pictbliz

object ControlChar {

  def build(c: Char, idx: Int): ControlChar = c match {
    case 'c' => CtrColor(idx)
    // case 's' => 
    case _ => CtrNop
  }
  
}

sealed trait ControlChar {
  
  val DEFAULT: ControlChar
  def isDefault = this == DEFAULT
  
}
case object CtrNop extends ControlChar {
  lazy val DEFAULT = CtrNop
}

case class CtrColor(idx: Int) extends ControlChar {
  lazy val DEFAULT = CtrColor(0)
  // def bind[T](implicit colormap: T) = {
  //    
  // }
}

case class AttributeRange(begin: Int, end: Int, ctr: ControlChar)

