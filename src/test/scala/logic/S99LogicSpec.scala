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
}
