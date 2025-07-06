package com.bcoromina.json.parsers

import com.bcoromina.json.JsonParser
import com.bcoromina.json.values.{JsonArray, JsonBoolean, JsonNumber, JsonOpenArray, JsonString}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JsonParserSpec extends AnyFunSpec with Matchers {
  describe("a boolean parser"){
    it("should parse a true"){
      val str = "true"
      JsonParser.booleanParser(str, 0) should contain ((JsonBoolean(true), "true".length))
    }
    it("should parse a false") {
      val str = "false"
      JsonParser.booleanParser(str, 0) should contain ((JsonBoolean(false), "false".length))
    }

    it("should fail when no boolean") {
      val str = "randomStr"
      JsonParser.booleanParser(str, 0) shouldBe None
    }
  }
  describe("array parser"){
    it("empty array"){
      JsonParser.arrayParser("[]", 0) should contain ((JsonArray(Nil),2))
    }
    
    it("number single array") {
      val expectedAst = JsonArray(
        JsonNumber("1") :: Nil
      )

      JsonParser.arrayParser("[1]", 0) should contain(expectedAst, 3)
    }

    it("number multiple array") {
      val expectedAst = JsonArray(
        JsonNumber("1") ::
          JsonNumber("2") ::
          JsonNumber("3") :: Nil
      )

      JsonParser.arrayParser("[1,2,3]", 0) should contain(expectedAst, 7)
    }
    it("boolean array"){
      val expectedAst = JsonArray(
        List(
          JsonBoolean(true),
          JsonBoolean(false),
          JsonBoolean(true),
        )
      )
      JsonParser.arrayParser("[true,false,true]", 0) should contain(expectedAst, 17)
    }

    it("nested arrays"){
      val expectedAst = JsonArray(
        List(
          JsonBoolean(true),
          JsonArray(
            JsonNumber("1") ::
              JsonNumber("2") ::
              JsonNumber("3") :: Nil
          ),
          JsonBoolean(false),
        )
      )
      JsonParser.arrayParser("[true,[1,2,3],false]", 0) should contain(expectedAst, 20)
    }
  }

  describe("string parser"){
    it("can parse alphanumeric string"){
      JsonParser.stringParser("\"hola123\"",0) should contain ( JsonString("hola123"), 9)
    }
  }
}
