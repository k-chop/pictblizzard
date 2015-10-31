package pictbliz
package ext

import java.io.File
import java.net.URI
import java.nio.file.{Path, Paths}

object FilePath {

  trait ToPath[T] {
    def toPath(from: T): Path
  }

  implicit val FileToPath = new ToPath[File] {
    def toPath(from: File) = from.toPath
  }
  implicit val UriToPath = new ToPath[URI] {
    def toPath(from: URI) = Paths.get(from)
  }
  implicit val StringToPath = new ToPath[String] {
    def toPath(from: String) = Paths.get(from)
  }
  // wtf
  implicit val PathToPath = new ToPath[Path] {
    def toPath(from: Path) = from
  }

  def trimPath(s: String): String = s.replaceAll("""\.png$""", "")

}
