import scala.util.Random

object WorkingWithLists {
  def last[T](x: List[T]): T = x.last

  def penultimate[T](x: List[T]): T = x.init.last

  def nth[T](n: Int, x: List[T]): T = x(n)

  def length[T](x: List[T]): Int = x.length

  def reverse[T](x: List[T]): List[T] = x.reverse

  def isPalindrome[T](x: List[T]): Boolean = x == x.reverse

  def flatten(x: List[Any]): List[Any] = {
    x.flatMap {
      case l: List[Any] => flatten(l)
      case e => List(e)
    }
  }

  def compress[T](x: List[T]): List[T] = compressRecurse(x, List.empty)

  private def compressRecurse[T](input: List[T], output: List[T]): List[T] = {
    input match {
      case a :: Nil => output :+ a
      case a :: b :: Nil if a == b => output :+ a
      case a :: b :: Nil => output ++ List(a, b)
      case a :: b :: tail if a == b => compressRecurse(a +: tail, output)
      case a :: b :: tail => compressRecurse(b +: tail, output :+ a)
    }
  }

  def pack[T](x: List[T]): List[List[T]] = packRecurse(x, List.empty)

  private def packRecurse[T](input: List[T], output: List[List[T]]): List[List[T]] = {
    val (prefix, suffix) = input.span(_ == input.head)

    val updatedOutput = output ++ List(prefix)

    if (suffix.isEmpty) updatedOutput
    else packRecurse(suffix, updatedOutput)
  }

  def encode[T](x: List[T]): List[(Int, T)] = pack(x).map(i => (i.length, i.head))

  def encodeModified[T](x: List[T]): List[Any] = encode(x).map {
    case (1, i) => i
    case other => other
  }

  def decode[T](x: List[(Int, T)]): List[T] = x.flatMap(i => List.fill(i._1)(i._2))

  def encodeDirect[T](x: List[T]): List[(Int, T)] = nextElement(x, List.empty)

  private def nextElement[T](input: List[T], output: List[(Int, T)]): List[(Int, T)] = {
    val (prefix, suffix) = input.span(_ == input.head)
    val entry = output ++ List((prefix.length, prefix.head))

    if (suffix.isEmpty) entry
    else nextElement(suffix, entry)
  }

  def duplicate[T](x: List[T]): List[T] = duplicateN(2, x)

  def duplicateN[T](count: Int, x: List[T]): List[T] = decode(x.map(i => (count, i)))

  def drop[T](n: Int, x: List[T]): List[T] = {
    x.sliding(n, n).map { l =>
      if (l.length == n) l.init
      else l
    }.toList.flatten
  }

  def split[T](n: Int, x: List[T]): (List[T], List[T]) = x.splitAt(n)

  def slice[T](i: Int, k: Int, x: List[T]): List[T] = x.slice(i, k)

  def rotate[T](n: Int, x: List[T]): List[T] = {
    if (0 < n) {
      val (l, r) = split(n, x)
      r ++ l
    }
    else if (0 > n) {
      val (l, r) = split(x.length - (n * -1), x)
      r ++ l
    }
    else x
  }

  def removeAt[T](n: Int, x: List[T]): (List[T], T) = {
    val (l, r) = x.splitAt(n + 1)

    (l.init ++ r, l.last)
  }

  def insertAt[T](t: T, at: Int, x: List[T]): List[T] = {
    val (l, r) = x.splitAt(at)

    (l :+ t) ++ r
  }

  def range(start: Int, end: Int): List[Int] = List.range(start, end + 1)

  def randomSelect[T](n: Int, x: List[T]): List[T] = randomSelectRecurse(x, new Random(), n, List.empty)

  private def randomSelectRecurse[T](input: List[T], rnd: Random, count: Int, output: List[T]): List[T] = {
    if (0 == count) output
    else {
      val (remainder, item) = removeAt(rnd.nextInt(input.length), input)

      randomSelectRecurse(remainder, rnd, count - 1, output :+ item)
    }
  }

  def lotto(n: Int, m: Int): List[Int] = randomSelect(n, range(1, m))

  def randomPermute[T](x: List[T]): List[T] = randomSelect(x.length, x)

  def combinations[T](n: Int, x: List[T]): List[List[T]] = {
    combinationsRecurse(n, x)
  }

  private def combinationsRecurse[T](n: Int, x: List[T]): List[List[T]] = {
    (n, x) match {
      case (0, _) => List(Nil)
      case (_, Nil) => Nil
      case _ =>
        val combinationsWithThisValue = combinationsRecurse(n - 1, x.tail).map(x.head :: _)
        val combinationsWithRemainingElements = combinationsRecurse(n, x.tail)

        combinationsWithThisValue ++ combinationsWithRemainingElements
    }
  }
}
