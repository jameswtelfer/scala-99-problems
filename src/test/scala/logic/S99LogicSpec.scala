package logic

import logic.S99Logic._
import org.scalatest.{FlatSpec, Matchers}

class S99LogicSpec extends FlatSpec with Matchers {
  "not" should "invert the input" in {
    S99Logic.not(true) should be(false)
    S99Logic.not(false) should be(true)
  }

  "and" should "output the correct logic" in {
    and(a = true, b = true) should be(true)
    and(a = true, b = false) should be(false)
    and(a = false, b = true) should be(false)
    and(a = false, b = false) should be(false)
  }

  "or" should "output the correct logic" in {
    or(a = true, b = true) should be(true)
    or(a = true, b = false) should be(true)
    or(a = false, b = true) should be(true)
    or(a = false, b = false) should be(false)
  }

  "nand" should "output the correct logic" in {
    nand(a = true, b = true) should be(false)
    nand(a = true, b = false) should be(true)
    nand(a = false, b = true) should be(true)
    nand(a = false, b = false) should be(true)
  }

  "nor" should "output the correct logic" in {
    nor(a = true, b = true) should be(false)
    nor(a = true, b = false) should be(false)
    nor(a = false, b = true) should be(false)
    nor(a = false, b = false) should be(true)
  }

  "xor" should "output the correct logic" in {
    xor(a = false, b = false) should be(false)
    xor(a = true, b = true) should be(false)
    xor(a = true, b = false) should be(true)
    xor(a = false, b = true) should be(true)
  }

  "impl" should "output the correct logic" in {
    impl(a = true, b = true) should be(true)
    impl(a = false, b = true) should be(true)
    impl(a = false, b = false) should be(true)
    impl(a = true, b = false) should be(false)
  }

  "equ" should "output the correct logic" in {
    equ(a = true, b = true) should be(true)
    equ(a = true, b = false) should be(false)
    equ(a = false, b = true) should be(false)
    equ(a = false, b = false) should be(true)
  }

  "class operator and" should "output the correct logic" in {
    t.and(true) should be(true)
    t.and(false) should be(false)
    f.and(true) should be(false)
    f.and(false) should be(false)
  }

  "class operator or" should "output the correct logic" in {
    t.or(true) should be(true)
    t.or(false) should be(true)
    f.or(true) should be(true)
    f.or(false) should be(false)
  }

  "class operator nand" should "output the correct logic" in {
    t.nand(true) should be(false)
    t.nand(false) should be(true)
    f.nand(true) should be(true)
    f.nand(false) should be(true)
  }

  "class operator nor" should "output the correct logic" in {
    t.nor(true) should be(false)
    t.nor(false) should be(false)
    f.nor(true) should be(false)
    f.nor(false) should be(true)
  }

  "class operator xor" should "output the correct logic" in {
    f.xor(false) should be(false)
    t.xor(true) should be(false)
    t.xor(false) should be(true)
    f.xor(true) should be(true)
  }

  "class operator impl" should "output the correct logic" in {
    t.impl(true) should be(true)
    f.impl(true) should be(true)
    f.impl(false) should be(true)
    t.impl(false) should be(false)
  }

  "class operator equ" should "output the correct logic" in {
    t.equ(true) should be(true)
    t.equ(false) should be(false)
    f.equ(true) should be(false)
    f.equ(false) should be(true)
  }

  val t = true
  val f = false
}
