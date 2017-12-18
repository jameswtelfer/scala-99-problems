package logic

import scala.language.implicitConversions

object S99Logic {
  /*
      P46 (**) Truth tables for logical expressions.
      Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
      scala> and(true, true)
      res0: Boolean = true

      scala> xor(true. true)
      res1: Boolean = false
      A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))

      Now, write a function called table2 which prints the truth table of a given logical expression in two variables.

      scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
      A     B     result
      true  true  true
      true  false true
      false true  false
      false false false
   */

  def not(a: Boolean): Boolean = {
    if (a) {
      false
    } else {
      true
    }
  }

  def and(a: Boolean, b: Boolean): Boolean = {
    (a, b) match {
      case (true, true) => true;
      case _ => false
    }
  }

  def or(a: Boolean, b: Boolean): Boolean = {
    (a, b) match {
      case (false, false) => false
      case _ => true
    }
  }

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))

  def xor(a: Boolean, b: Boolean): Boolean = and(or(a, b), nand(a, b))

  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

  def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))

  def table2(func: (Boolean, Boolean) => Boolean): Unit = {
    println("A     B     result")
    println(s"true  true  ${func(true, true)}")
    println(s"true  false  ${func(true, false)}")
    println(s"false  true  ${func(false, true)}")
    println(s"false  false  ${func(false, false)}")
  }

  /*
    P47 (*) Truth tables for logical expressions (2).
    Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
    scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
    A     B     result
    true  true  true
    true  false true
    false true  false
    false false false
   */
  class S99Logic(value: Boolean) {
    def and(other: Boolean): Boolean = {
      (value, other) match {
        case (true, true) => true;
        case _ => false
      }
    }

    def or(other: Boolean): Boolean = {
      (value, other) match {
        case (false, false) => false
        case _ => true
      }
    }

    def nand(other: Boolean): Boolean = not(and(other))

    def nor(other: Boolean): Boolean = not(or(other))

    def xor(other: Boolean): Boolean = or(other).and(nand(other))

    def impl(other: Boolean): Boolean = not(value).or(other)

    def equ(other: Boolean): Boolean = and(other).or(not(value).and(not(other)))
  }

  implicit def boolToLogic(value: Boolean): S99Logic = new S99Logic(value)
}
