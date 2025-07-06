package com.bcoromina.json

import com.bcoromina.base_parsers.BaseParser.*
import com.bcoromina.base_parsers.*
import com.bcoromina.json.values.{JsonArray, JsonBoolean, JsonCloseArray, JsonElementSep, JsonNumber, JsonOpenArray, JsonValue}

object JsonParser:
  private val trueParser: Parser[JsonBoolean] = tokenParser("true", JsonBoolean(true))
  private val falseParser: Parser[JsonBoolean] = tokenParser("false", JsonBoolean(false))
  val booleanParser: Parser[JsonBoolean] = trueParser or falseParser
  val numberParser: Parser[JsonValue] = baseNumberParser.map(n => JsonNumber(n.toString))

  //recursive parser definition
  val elementParser = BaseParser.defer(numberParser or booleanParser or arrayParser)

  // Array
  val openArrayParser: Parser[JsonValue] = tokenParser("[", JsonOpenArray)
  val closeArrayParser: Parser[JsonValue] = tokenParser("]", JsonCloseArray)

  val arraySeparationParser: Parser[JsonValue] = tokenParser(",", JsonElementSep)

  val contentRep: Parser[List[JsonValue]] = (elementParser <* arraySeparationParser).rep

  val arrContent: Parser[List[JsonValue]] =
    (contentRep andThen elementParser.list).combineResult

  val nonEmptyArrayParser: Parser[JsonArray] = (
    openArrayParser *> arrContent <* closeArrayParser).map(JsonArray.apply)

  val emptyArrayParser = (openArrayParser andThen closeArrayParser)
     .map( _ => JsonArray(Nil))
   
  val arrayParser = emptyArrayParser or nonEmptyArrayParser

