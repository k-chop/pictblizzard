package pictbliz

import com.typesafe.scalalogging.{LazyLogging, Logger}
import org.slf4j.{Marker, Logger => Underlying}

trait DisableLogging {
  this: LazyLogging =>
  override lazy val logger = Logger(new NoLogging)
}

class NoLogging extends Underlying {

  override def warn(s: String): Unit = {}

  override def warn(s: String, o: scala.Any): Unit = {}

  override def warn(s: String, objects: AnyRef*): Unit = {}

  override def warn(s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def warn(s: String, throwable: Throwable): Unit = {}

  override def warn(marker: Marker, s: String): Unit = {}

  override def warn(marker: Marker, s: String, o: scala.Any): Unit = {}

  override def warn(marker: Marker, s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def warn(marker: Marker, s: String, objects: AnyRef*): Unit = {}

  override def warn(marker: Marker, s: String, throwable: Throwable): Unit = {}

  override def isErrorEnabled: Boolean = false

  override def isErrorEnabled(marker: Marker): Boolean = false

  override def getName: String = "NoLogging"

  override def isInfoEnabled: Boolean = false

  override def isInfoEnabled(marker: Marker): Boolean = false

  override def isDebugEnabled: Boolean = false

  override def isDebugEnabled(marker: Marker): Boolean = false

  override def isTraceEnabled: Boolean = false

  override def isTraceEnabled(marker: Marker): Boolean = false

  override def error(s: String): Unit = {}

  override def error(s: String, o: scala.Any): Unit = {}

  override def error(s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def error(s: String, objects: AnyRef*): Unit = {}

  override def error(s: String, throwable: Throwable): Unit = {}

  override def error(marker: Marker, s: String): Unit = {}

  override def error(marker: Marker, s: String, o: scala.Any): Unit = {}

  override def error(marker: Marker, s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def error(marker: Marker, s: String, objects: AnyRef*): Unit = {}

  override def error(marker: Marker, s: String, throwable: Throwable): Unit = {}

  override def debug(s: String): Unit = {}

  override def debug(s: String, o: scala.Any): Unit = {}

  override def debug(s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def debug(s: String, objects: AnyRef*): Unit = {}

  override def debug(s: String, throwable: Throwable): Unit = {}

  override def debug(marker: Marker, s: String): Unit = {}

  override def debug(marker: Marker, s: String, o: scala.Any): Unit = {}

  override def debug(marker: Marker, s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def debug(marker: Marker, s: String, objects: AnyRef*): Unit = {}

  override def debug(marker: Marker, s: String, throwable: Throwable): Unit = {}

  override def isWarnEnabled: Boolean = false

  override def isWarnEnabled(marker: Marker): Boolean = false

  override def trace(s: String): Unit = {}

  override def trace(s: String, o: scala.Any): Unit = {}

  override def trace(s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def trace(s: String, objects: AnyRef*): Unit = {}

  override def trace(s: String, throwable: Throwable): Unit = {}

  override def trace(marker: Marker, s: String): Unit = {}

  override def trace(marker: Marker, s: String, o: scala.Any): Unit = {}

  override def trace(marker: Marker, s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def trace(marker: Marker, s: String, objects: AnyRef*): Unit = {}

  override def trace(marker: Marker, s: String, throwable: Throwable): Unit = {}

  override def info(s: String): Unit = {}

  override def info(s: String, o: scala.Any): Unit = {}

  override def info(s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def info(s: String, objects: AnyRef*): Unit = {}

  override def info(s: String, throwable: Throwable): Unit = {}

  override def info(marker: Marker, s: String): Unit = {}

  override def info(marker: Marker, s: String, o: scala.Any): Unit = {}

  override def info(marker: Marker, s: String, o: scala.Any, o1: scala.Any): Unit = {}

  override def info(marker: Marker, s: String, objects: AnyRef*): Unit = {}

  override def info(marker: Marker, s: String, throwable: Throwable): Unit = {}
}
