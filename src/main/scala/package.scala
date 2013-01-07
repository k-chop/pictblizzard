package com.github.whelmaze.pictbliz

import grizzled.slf4j.Logger

object `package` {

  lazy val logger: Logger = {
    val ret = Logger("GLOBAL")
    ret
  }
}
