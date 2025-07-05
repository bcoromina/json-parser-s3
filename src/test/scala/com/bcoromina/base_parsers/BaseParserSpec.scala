package com.bcoromina.base_parsers

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class BaseParserSpec extends AnyFunSpec with Matchers{

  describe("a token parser"){
    val token = "token"
    val tokenParser = BaseParser.tokenParser(token, ())
    it("match token with no offset") {
      tokenParser(token, 0) should contain ((), token.length)
    }

    it("No match ") {
      tokenParser("random", 0) shouldBe None
    }

    it("match token with offset"){
      val prefix = "abcd"
      val str = prefix + token
      tokenParser(str, prefix.length) should contain ((), prefix.length + token.length)
    }
  }

  describe("or combinator"){
    case class A()
    case class B()
    val aOrbParser = BaseParser.tokenParser("a", A()) or BaseParser.tokenParser("b", B())
    it("should match the first parser"){
      aOrbParser("a",0) should contain (A(), 1)
    }
    it("should match the second parser") {
      aOrbParser("b", 0) should contain(B(), 1)
    }
    it("should match no parser") {
      aOrbParser("c", 0) shouldBe None
    }
  }

  describe("andThen combinator"){
    case class A()
    case class B()
    val combinedParser = BaseParser.tokenParser("a", A()) andThen BaseParser.tokenParser("b", B())

    it("should match when both tokens"){
      combinedParser("ab", 0) should contain ((A(),B()),2)
    }
    it("should fail when matches the first but not the second"){
      combinedParser("ac", 0) shouldBe None
    }
    it("should fail when no match"){
      combinedParser("nomatch", 0) shouldBe None
    }
  }
}
