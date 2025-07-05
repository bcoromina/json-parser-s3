package com.bcoromina.json.parsers

import com.bcoromina.json.{JsonBoolean, JsonParser}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JsonParserSpec extends AnyFunSpec with Matchers {
  describe("a boolean parser"){
    it("should parse a true"){
      val str = "true"
      JsonParser.parseBoolean(str, 0) should contain ((JsonBoolean(true), "true".length))
    }
    it("should parse a false") {
      val str = "false"
      JsonParser.parseBoolean(str, 0) should contain((JsonBoolean(false), "false".length))
    }

    it("should fail when no boolean") {
      val str = "randomStr"
      JsonParser.parseBoolean(str, 0) shouldBe None
    }
  }
}
