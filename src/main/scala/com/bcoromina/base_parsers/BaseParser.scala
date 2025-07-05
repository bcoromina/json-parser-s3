package com.bcoromina.base_parsers

import com.bcoromina.base_parsers.BaseParser.Parser

object BaseParser:
  type Parser[T] = (String, Int) => Option[(T, Int)]

  def tokenParser[T](token: String, jsonValue: T): Parser[T] =
    (str, pos) => if (str.startsWith(token, pos))
      Some(jsonValue, pos + token.length)
    else None

object ParserCombinators:
  def or[A,B](pa: Parser[A], pb: Parser[B]): Parser[A|B] =
    (str, pos) => pa(str,pos) match
      case r@Some((a,pos)) => r
      case None => pb(str,pos)

  def andThen[A,B](pa: Parser[A], pb: Parser[B]): Parser[(A,B)] =
    (str, pos) =>
      for
        ra <- pa(str, pos)
        rb <- pb(str, ra._2)
      yield ((ra._1, rb._1), rb._2)


extension [A](pa: Parser[A])
  infix def or[B](pb: Parser[B]): Parser[A|B] = ParserCombinators.or(pa,pb)

extension [A,B](pa: Parser[A])
  infix def andThen(pb: Parser[B]): Parser[(A,B)] = ParserCombinators.andThen(pa,pb)