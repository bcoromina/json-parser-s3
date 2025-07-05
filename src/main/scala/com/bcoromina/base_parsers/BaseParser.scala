package com.bcoromina.base_parsers

import com.bcoromina.base_parsers.BaseParser.Parser

import scala.annotation.tailrec

object BaseParser:
  type Parser[T] = (String, Int) => Option[(T, Int)]

  def tokenParser[T](token: String, astValue: T): Parser[T] =
    (str, pos) => if (str.startsWith(token, pos))
      Some(astValue, pos + token.length)
    else
      None

  val baseNumberParser : Parser[Int] =
    (str, pos) =>
      if(pos < str.length && ( str(pos).isDigit || str(pos) == '-'))
        @tailrec
        def loop(i: Integer, acc: List[Char]): List[Char] =
          if(i == str.length || !str(i).isDigit )
            acc
          else
            loop(i + 1, str(i) :: acc)
        val rslt = loop(pos +1, str(pos) :: Nil)
        Some((rslt.reverse.mkString.toInt, pos + rslt.length))
      else None

object ParserCombinators:
  def or[A,B](pa: Parser[A], pb: Parser[B]): Parser[A|B] =
    (str, pos) => pa(str,pos) match
      case r@Some((a,p)) => r
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

extension [A](pa: Parser[A])
  infix def map[B](f: A => B): Parser[B] =
    (str, pos) => pa(str, pos) match
      case Some((a,p)) => Some((f(a), p))
      case None => None

extension [A](p: Parser[A])
  def rep: Parser[List[A]] =
    (input, pos) =>
      @tailrec
      def loop(acc: List[A], currentPos: Int): Option[(List[A], Int)] =
        p(input, currentPos) match
          case Some((a, nextPos)) => loop(acc :+ a, nextPos)
          case None               => Some((acc, currentPos))

      loop(Nil, pos)