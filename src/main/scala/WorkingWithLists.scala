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

  def compress[T](x: List[T]): List[T] = {
    x match {
      case a :: Nil => List(a)
      case a :: b :: Nil if a == b => List(a)
      case a :: b :: Nil => List(a, b)
      case a :: b :: tail if a == b => compress(a +: tail)
      case a :: b :: tail => a +: compress(b +: tail)
    }
  }

  def pack[T](x: List[T]): List[List[T]] = {
    val (prefix, suffix) = x.span(_ == x.head)
    if (suffix.isEmpty) List(prefix)
    else List(prefix) ++ pack(suffix)
  }

  def encode[T](x: List[T]): List[(Int, T)] = pack(x).map(i => (i.length, i.head))

  def encodeModified[T](x: List[T]): List[Any] = encode(x).map {
    case (1, i) => i
    case other => other
  }

  def decode[T](x: List[(Int, T)]): List[T] = x.flatMap(i => List.fill(i._1)(i._2))

  def encodeDirect[T](x: List[T]): List[(Int, T)] = {
    def nextElement(input: List[T], output: List[(Int, T)]): List[(Int, T)] = {
      val (prefix, suffix) = input.span(_ == input.head)
      val entry = output ++ List((prefix.length, prefix.head))

      if (suffix.isEmpty) entry
      else nextElement(suffix, entry)
    }

    nextElement(x, List.empty)
  }
}
