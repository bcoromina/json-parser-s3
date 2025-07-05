package com.bcoromina.json

import com.bcoromina.base_parsers.BaseParser.*
import com.bcoromina.base_parsers.*

object JsonParser:
  val trueParser: Parser[JsonBoolean] = tokenParser("true", JsonBoolean(true))
  val falseParser: Parser[JsonBoolean] = tokenParser("false", JsonBoolean(false))
  val parseBoolean: Parser[JsonBoolean] = trueParser or falseParser
  