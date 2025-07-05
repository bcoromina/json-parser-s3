// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import org.scalatest.funsuite.AnyFunSuite

class JsonParserSpec extends AnyFunSuite {

  test("2 + 2 should equal 4") {
    assert(2 + 2 == 4)
  }

  test("string concatenation") {
    val greeting = "Hello, " + "world!"
    assert(greeting == "Hello, world!")
  }
}
