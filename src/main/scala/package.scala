package com.github.whelmaze.pictbliz

import com.twitter.logging.config._
import com.twitter.logging.Logger

object `package` {

  lazy val logger: Logger = {
    val ret = Logger.get("GLOBAL")
    ret.setLevel(Level.INFO)
    ret
  }
}
