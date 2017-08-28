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
}
