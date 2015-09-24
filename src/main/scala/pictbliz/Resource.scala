package pictbliz

object Resource {

  val loader = getClass.getClassLoader
  val root = loader.getResource(".")

  def uri(path: String) = loader.getResource(path).toURI
  def str(path: String) = loader.getResource(path).getPath

  val tempdir = "./temp/"
  
}
