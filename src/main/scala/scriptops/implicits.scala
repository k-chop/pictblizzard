package com.github.whelmaze.pictbliz.scriptops

import com.github.whelmaze.pictbliz.Resource

object implicits {
  implicit def string2URI(s: String): java.net.URI =
    Resource.uri(s)
}