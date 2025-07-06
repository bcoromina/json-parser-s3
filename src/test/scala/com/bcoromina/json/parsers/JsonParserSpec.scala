package com.bcoromina.json.parsers

import com.bcoromina.json.JsonParser
import com.bcoromina.json.values.{JsonArray, JsonBoolean, JsonNumber, JsonObject, JsonOpenArray, JsonString}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

class JsonParserSpec extends AnyFunSpec with Matchers {
  describe("a boolean parser"){
    it("should parse a true"){
      val str = "true"
      JsonParser.parse(str) should contain ((JsonBoolean(true), "true".length))
    }
    it("should parse a false") {
      val str = "false"
      JsonParser.parse(str) should contain ((JsonBoolean(false), "false".length))
    }

    it("should fail when no boolean") {
      val str = "randomStr"
      JsonParser.parse(str) shouldBe None
    }
  }
  describe("array parser"){
    it("empty array"){
      JsonParser.parse("[]") should contain ((JsonArray(Nil),2))
    }
    
    it("number single array") {
      val expectedAst = JsonArray(
        JsonNumber("1") :: Nil
      )

      JsonParser.parse("[1]") should contain(expectedAst, 3)
    }

    it("number multiple array") {
      val expectedAst = JsonArray(
        JsonNumber("1") ::
          JsonNumber("2") ::
          JsonNumber("3") :: Nil
      )

      JsonParser.parse("[1,2,3]") should contain(expectedAst, 7)
    }
    it("boolean array"){
      val expectedAst = JsonArray(
        List(
          JsonBoolean(true),
          JsonBoolean(false),
          JsonBoolean(true),
        )
      )
      JsonParser.parse("[true,false,true]") should contain(expectedAst, 17)
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
      JsonParser.parse("[true,[1,2,3],false]") should contain(expectedAst, 20)
    }
  }

  describe("string parser"){
    it("can parse alphanumeric string"){
      JsonParser.parse("\"hola123\"") should contain ( JsonString("hola123"), 9)
    }
  }

  describe("object parser"){
    it("parse empty object"){
      JsonParser.parse("{}") should contain (JsonObject(ListMap.empty), 2)
    }

    it("parse single element object"){
      val expectedAst = JsonObject(
        ListMap(("age", JsonNumber("23")))
      )

      JsonParser.parse(
        """{"age":23}"""
      ) should contain (expectedAst, 10)
    }

    it("parse multiple element object") {
      val expectedAst = JsonObject(
        ListMap(
          ("name", JsonString("Joe")),
          ("age", JsonNumber("23"))
        )
      )

      JsonParser.parse(
        """{"name":"Joe","age":23}"""
      ) should contain(expectedAst, 23)
    }

    it("array nested in object"){
      val expectedAst = JsonObject(
        ListMap(
          ("name", JsonString("Joe")),
          ("age", JsonNumber("23")),
          ("items", JsonArray(
                        JsonNumber("1") ::
                        JsonNumber("2") ::
                        JsonNumber("3") :: Nil
                    )
          )
        )
      )

      JsonParser.parse(
        """{"name":"Joe","age":23,"items":[1,2,3]}"""
      ) should contain(expectedAst, 39)
    }
  }
}
