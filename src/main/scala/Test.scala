package com.github.whelmaze.pictbliz

import com.typesafe.scalalogging.LazyLogging
import sample.TestSpec
import java.io.File

object Test extends App with LazyLogging {
  val f = new File(Resource.tempdir).listFiles()
  f withFilter (!_.isDirectory) foreach { fn =>
    logger.trace("%s was deleted." format fn.getCanonicalPath)
    fn.delete()
  }
  new TestSpec().run()
}
