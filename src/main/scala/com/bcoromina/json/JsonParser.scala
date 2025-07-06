package com.bcoromina.json

import com.bcoromina.base_parsers.BaseParser.*
import com.bcoromina.base_parsers.*
import com.bcoromina.json.values.{JsonArray, JsonBoolean, JsonCloseArray, JsonCloseObject, JsonElementSep, JsonNumber, JsonObject, JsonOpenArray, JsonOpenObject, JsonString, JsonValue}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

object JsonParser:

  def parse(str: String): Option[(JsonValue, Int)] = elementParser(str, 0)

  private val trueParser: Parser[JsonBoolean] = tokenParser("true", JsonBoolean(true))
  private val falseParser: Parser[JsonBoolean] = tokenParser("false", JsonBoolean(false))
  val booleanParser: Parser[JsonBoolean] = trueParser or falseParser
  val numberParser: Parser[JsonValue] = baseNumberParser.map(n => JsonNumber(n.toString))

  //recursive parser definition
  val elementParser = BaseParser.defer(
    numberParser or booleanParser or arrayParser or objectParser or stringParser or objectParser
  )

  // Array
  val openArrayParser: Parser[JsonValue] = tokenParser("[", JsonOpenArray)
  val closeArrayParser: Parser[JsonValue] = tokenParser("]", JsonCloseArray)

  val commaSepParser: Parser[JsonValue] = tokenParser(",", JsonElementSep)

  val contentRep: Parser[List[JsonValue]] = (elementParser <* commaSepParser).rep

  val arrContent: Parser[List[JsonValue]] =
    (contentRep andThen elementParser.list).combineResult

  val nonEmptyArrayParser: Parser[JsonArray] = (
    openArrayParser *> arrContent <* closeArrayParser).map(JsonArray.apply)

  val emptyArrayParser = (openArrayParser andThen closeArrayParser)
     .map( _ => JsonArray(Nil))
   
  val arrayParser = emptyArrayParser or nonEmptyArrayParser

  // string
  val stringParser: Parser[JsonValue] =
    (str, pos) =>
      if(matchToken(str, "\"", pos))
        @tailrec
        def loop(s: String, i: Int, acc: List[Char]): Option[(JsonValue,Int)] =
          if(s.length == i ) None
          else
            s(i) match
              case '\"' =>
                Some((JsonString(acc.mkString), i+1))
              case c =>
                loop(s, i + 1, acc :+ c)
        loop(str,pos + 1, Nil)
      else None

  // object
  val openObjectParser: Parser[JsonValue] = tokenParser("{", JsonOpenObject)
  val closeObjectParser: Parser[JsonValue] = tokenParser("}", JsonCloseObject)

  val objectSepParser = tokenParser(":", JsonElementSep)

  val oneElementContent: Parser[(JsonValue,JsonValue)] = stringParser <* objectSepParser andThen elementParser
  val objOneElementParser: Parser[JsonObject] =
    (openObjectParser *>
      oneElementContent
      <* closeObjectParser).map{case (JsonString(value),v) => JsonObject(ListMap((value,v)))}

  val multipleElementContent: Parser[List[(JsonValue, JsonValue)]] = ((oneElementContent <* commaSepParser).rep andThen oneElementContent.list).combineResult
  val objMultElementParser: Parser[JsonObject] =
    (openObjectParser *> multipleElementContent <* closeObjectParser
    ).map { r =>
      JsonObject(ListMap(r.map { case (JsonString(value), v) => (value, v) }*))
    }

  val emptyObjectParser: Parser[JsonObject] =
    (openObjectParser andThen closeObjectParser).map(_ => JsonObject(ListMap.empty))

  val objectParser: Parser[JsonValue] = emptyObjectParser or objOneElementParser or objMultElementParser

