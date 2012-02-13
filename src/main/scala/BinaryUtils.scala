package com.github.chuwb.pictbliz

object BinaryUtils {
  
    def byte(arr: IndexedSeq[Int]) = arr.map{ _.toByte }
    
    def unsign(b: Byte): Int = b & 0xFF
    
    val PNG_IDENTIFER: Array[Byte] =
      byte( Array(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a) ).toArray

    def DWORD2Long(arr: Array[Byte]): Long = {
      if(arr.length != 4)
        throw new IllegalArgumentException("DWORD2Long accept only Array length 4.")
      val b = arr.map{ i => if (i < 0) (i+256).toLong else i.toLong }
      0L | b(0)<<24 | b(1)<<16 | b(2)<<8 | b(3)
    }
}