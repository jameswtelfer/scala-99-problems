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

  "duplicate" should "duplicate each entry in a list" in {
    WorkingWithLists.duplicate(List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  "duplicateN" should "duplicate each entry in a list N times" in {
    WorkingWithLists.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  "drop" should "drop every Nth element from the list" in {
    WorkingWithLists.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  "split" should "split a list into two parts, with a fixed size first part" in {
    WorkingWithLists.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  "slice" should "take a slice of a list" in {
    WorkingWithLists.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g))
  }

  behavior of "rotate"

  it should "rotate the list N places to the left" in {
    WorkingWithLists.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  }

  it should "rotate the list -N places to the right" in {
    WorkingWithLists.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }

  "removeAt" should "remove the Nth element from the list" in {
    WorkingWithLists.removeAt(1, List('a, 'b, 'c, 'd)) should be((List('a, 'c, 'd), 'b))
  }

  "insertAt" should "insert the element at the Nth position" in {
    WorkingWithLists.insertAt('new, 1, List('a, 'b, 'c, 'd)) should be(List('a, 'new, 'b, 'c, 'd))
  }

  "range" should "create a list of all numbers in the given range" in {
    WorkingWithLists.range(4, 9) should be(List(4, 5, 6, 7, 8, 9))
  }

  "randomSelect" should "extract N random elements from a list" in {
    WorkingWithLists.randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)).length should be(3)
  }

  "lotto" should "draw N random numbers from the range 1 to M" in {
    WorkingWithLists.lotto(6, 49).length should be(6)
  }

  "randomPermute" should "generate a random permutation of the input list" in {
    val inputList = List('a, 'b, 'c, 'd, 'e, 'f)

    val outputList = WorkingWithLists.randomPermute(inputList)
    outputList should contain theSameElementsAs inputList
    outputList shouldNot be(inputList)
  }

  behavior of "combinations"

  it should "pick the only possible combination from a 2-element list" in {
    WorkingWithLists.combinations(2, List('a, 'b)) should be(List(List('a, 'b)))
  }

  it should "generate a list of all possible permutations from an input list" in {
    WorkingWithLists.combinations(2, List(1, 2, 3)) should be(List(List(1, 2), List(1, 3), List(2, 3)))
  }

  it should "work for any length of inputs" in {
    WorkingWithLists.combinations(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l)).length should be(220)
  }
}
