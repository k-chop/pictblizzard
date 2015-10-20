package pictbliz
package extractor

trait Extractor[T, A <: Query[T]] {

  def execute(path: String, query: A): String
}

trait Query[T]