package com.bcoromina.json

import com.bcoromina.base_parsers.BaseParser.*
import com.bcoromina.base_parsers.*
import com.bcoromina.json.values.{JsonArray, JsonBoolean, JsonCloseArray, JsonElementSep, JsonNumber, JsonOpenArray, JsonValue}

object JsonParser:
  val trueParser: Parser[JsonBoolean] = tokenParser("true", JsonBoolean(true))
  val falseParser: Parser[JsonBoolean] = tokenParser("false", JsonBoolean(false))
  val parseBoolean: Parser[JsonBoolean] = trueParser or falseParser
  val parseNumber: Parser[JsonValue] = baseNumberParser.map(n => JsonNumber(n.toString))

  val startArrayParser: Parser[JsonValue] = tokenParser("[", JsonOpenArray)
  val endArrayParser: Parser[JsonValue] = tokenParser("]", JsonCloseArray)

  lazy val arraySeparationParser: Parser[JsonValue] = tokenParser(",", JsonElementSep)

  
  val contentRep = (parseNumber andThen arraySeparationParser).rep.map(_.map(_._1))
  val arrContent: Parser[List[JsonValue]] =
    (contentRep andThen parseNumber)
      .map{
        case (l,e) => l ++ List(e)
      }
  val nonEmptyArrayParser: Parser[JsonArray] = (startArrayParser andThen arrContent andThen endArrayParser)
    .map{
      case ((_,c),_) => JsonArray(c)
    }

   val emptyArrayParser = (startArrayParser andThen endArrayParser)
     .map( _ => JsonArray(Nil))
   
  val arrayParser = emptyArrayParser or nonEmptyArrayParser

