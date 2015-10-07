package pictbliz

import org.scalatest.{BeforeAndAfter, WordSpec, Matchers}

object `package` {

  abstract class UnitSpec extends WordSpec with Matchers with BeforeAndAfter

}
