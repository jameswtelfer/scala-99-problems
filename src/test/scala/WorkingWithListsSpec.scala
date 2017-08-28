import org.scalatest.{FlatSpec, Matchers}

class WorkingWithListsSpec extends FlatSpec with Matchers {
  "last" should "return the value of the last element in the list" in {
    WorkingWithLists.last(List(1, 1, 2, 3, 5, 8)) should be(8)
  }

  "penultinmate" should "return the last-but-one element in the list" in {
    WorkingWithLists.penultimate(List(1, 1, 2, 3, 5, 8)) should be(5)
  }

  "nth" should "return the selected element from the input list" in {
    WorkingWithLists.nth(2, List(1, 1, 2, 3, 5, 8)) should be(2)
  }

  "length" should "return the number of items in the list" in {
    WorkingWithLists.length(List(1, 1, 2, 3, 5, 8)) should be(6)
  }

  "reverse" should "return the same elements in opposite order" in {
    WorkingWithLists.reverse(List(1, 1, 2, 3, 5, 8)) should be(List(8, 5, 3, 2, 1, 1))
  }

  behavior of "isPalindrome"

  it should "return true if the list is a palindrome" in {
    WorkingWithLists.isPalindrome(List(1, 2, 3, 2, 1)) should be(true)
  }

  it should "return false if the list is not a palindrome" in {
    WorkingWithLists.isPalindrome(List(1, 2, 3, 4)) should be(false)
  }

  "flatten" should "flatten nested lists" in {
    WorkingWithLists.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be(List(1, 1, 2, 3, 5, 8))
  }

  "compress" should "remove duplicate adjacent elements" in {
    WorkingWithLists.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List('a, 'b, 'c, 'a, 'd, 'e))
  }

  "pack" should "pack consecutive matching elements into sub-lists" in {
    WorkingWithLists.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  "encode" should "produce the run-length encoding of a list" in {
    WorkingWithLists.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

  "encodeModified" should "produce the modified run-length encoding of a list" in {
    WorkingWithLists.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)))
  }

  "decode" should "decode a run-length encoded list" in {
    WorkingWithLists.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "encodeDirect" should "encode a list with run-length encoding without the use of other methods" in {
    WorkingWithLists.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }
}
