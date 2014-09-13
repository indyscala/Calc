package org.indyscala.parboiled

import org.parboiled2._

import scala.util.{Failure, Success}
import scala.math._

/**
 * Created by petarvlahu on 02.9.14.
 */
object Calc{
  def exec(line: String): Double = {
    eval(new Calc(line).InputLine.run().get)
  }

  def eval(expr: Expr): Double =
    expr match {
      case Value(v) => v.replace(",",".").toDouble
      case Addition(a, b) => eval(a) + eval(b)
      case Subtraction(a, b) => eval(a) - eval(b)
      case Multiplication(a, b) => eval(a) * eval(b)
      case Division(a, b) => eval(a) / eval(b)
      case Power(a, b) => Math.pow(eval(a), eval(b))
      case Sqrt(e) => Math.sqrt(eval(e)).toInt
    }

  // our abstract syntax tree model
  sealed trait Expr
  case class Value(value: String) extends Expr
  case class Addition(lhs: Expr, rhs: Expr) extends Expr
  case class Subtraction(lhs: Expr, rhs: Expr) extends Expr
  case class Multiplication(lhs: Expr, rhs: Expr) extends Expr
  case class Division(lhs: Expr, rhs: Expr) extends Expr
  case class Power(lhs: Expr, rhs: Expr) extends Expr
  case class Sqrt(e:Expr) extends Expr
}

class Calc(val input: ParserInput) extends Parser {
  import Calc._

  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> Addition |
      '-' ~ Term ~> Subtraction)
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> Multiplication |
      '/' ~ Factor ~> Division |
      '^' ~ Factor ~> Power)
  }

  def Factor = rule { Parens | Root | Number }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number = rule { capture(Digits) ~> Value }

  def Digits = rule {
      zeroOrMore(CharPredicate.Digit) ~ optional(Frac)
  }

  def Frac = rule {
    DecimalPoint ~ oneOrMore(CharPredicate.Digit)
  }

  def DecimalPoint = rule {
    ch('.') | ch(',')
  }

  def Root = rule { "sqrt(" ~ Expression ~ ')' ~> Sqrt }
}

