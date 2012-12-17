package com.github.whelmaze.pictbliz

import sample.TestSpec
import java.io.File

object Test extends App {
  val f = (new File(Resource.tempdir)).listFiles()
  f withFilter (!_.isDirectory) foreach { fn =>
    logger.trace("%s was deleted." format fn.getCanonicalPath)
    fn.delete()
  }
  (new TestSpec()).run()
}
