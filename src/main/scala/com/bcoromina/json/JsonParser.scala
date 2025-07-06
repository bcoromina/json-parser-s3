package com.bcoromina.json

import com.bcoromina.base_parsers.BaseParser.*
import com.bcoromina.base_parsers.*
import com.bcoromina.json.values.{JsonArray, JsonBoolean, JsonCloseArray, JsonElementSep, JsonNumber, JsonOpenArray, JsonValue}

object JsonParser:
  private val trueParser: Parser[JsonBoolean] = tokenParser("true", JsonBoolean(true))
  private val falseParser: Parser[JsonBoolean] = tokenParser("false", JsonBoolean(false))
  val booleanParser: Parser[JsonBoolean] = trueParser or falseParser
  val numberParser: Parser[JsonValue] = baseNumberParser.map(n => JsonNumber(n.toString))

  val openArrayParser: Parser[JsonValue] = tokenParser("[", JsonOpenArray)
  val closeArrayParser: Parser[JsonValue] = tokenParser("]", JsonCloseArray)

  val arraySeparationParser: Parser[JsonValue] = tokenParser(",", JsonElementSep)

  val arrayElement = numberParser or booleanParser
  
  val contentRep = (arrayElement andThen arraySeparationParser).rep.map(_.map(_._1))
  val arrContent: Parser[List[JsonValue]] =
    (contentRep andThen arrayElement)
      .map{
        case (l,e) => l ++ List(e)
      }
  val nonEmptyArrayParser: Parser[JsonArray] = (openArrayParser andThen arrContent andThen closeArrayParser)
    .map{
      case ((_,c),_) => JsonArray(c)
    }

   val emptyArrayParser = (openArrayParser andThen closeArrayParser)
     .map( _ => JsonArray(Nil))
   
  val arrayParser = emptyArrayParser or nonEmptyArrayParser

