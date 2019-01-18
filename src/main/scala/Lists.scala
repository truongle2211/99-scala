package Problems

object Lists {
  def last[A](xs: List[A]): A = xs match {
    case h::Nil => h
    case h::tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate[A](xs: List[A]): A = xs match {
    case h::_::Nil => h
    case h::tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def nth[A](n: Int, xs: List[A]): A = (n, xs) match {
    case (0, h::_) => h
    case (n, xs) => nth(n-1, xs.tail)
    case _ => throw new NoSuchElementException
  }

  def length[A](xs: List[A]): Int = xs match {
    case h::Nil => 1
    case _::tail => 1 + length(tail)
  }

  def reverse[A](xs: List[A]): List[A] = xs.foldLeft(List[A]()){ (xs, acc) => acc :: xs}

  def isPalindrome[A](xs: List[A]): Boolean = xs == xs.reverse

  def flatten[A](xs: List[A]): List[A] = xs flatten {
    case h: List[A] => flatten(h)
    case e => List(e)
  }

  def compress[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h::tail => h::compress(xs.filter(a => a != h))
  }

  def pack[A](xs:List[A]): List[List[A]] = xs match {
    case Nil => List(List())
    case a::tail => {
      var packed = xs.filter(h => h== a)
      var left = xs.filter(h => h!= a)
      if (left == Nil) List(packed)
      else packed::pack(left)
    }
  }

  def encode[A](xs:List[A]): List[(Int, A)] = pack(xs) map {e => (e.length, e.head)}

  def encodeModified[A](xs: List[A]): List[Either[A, (Int, A)]] =
    pack(xs) map {e =>
      e.length match {
        case 1 => Left(e.head)
        case _ => Right((e.length, e.head))
  }}

  def decode[A](xs:List[(Int, A)]): List[A] = xs flatMap{ x => List.fill(x._1)(x._2)}


  def encodeDirect[A](xs:List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case a::tail => {
      var packed = xs.filter(h => h == a)
      var left = xs.filter(h => h!= a)

      (packed.length, a)::encodeDirect(left)
    }
  }

  def duplicate[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case a::tail => a::a::duplicate(tail)
  }

  def duplicateN[A](n: Int, xs: List[A]): List[A] = xs flatMap { x => List.fill(n)(x)}

  def drop[A](n:Int, xs:List[A]):List[A] = {
    def dropStep[A](c: Int, xs: List[A]): List[A] = (c, xs) match {
      case (_, Nil) => Nil
      case (1, _) => dropStep(n, xs.tail)
      case (_, h::tail) => h::dropStep(c-1, tail)
    }
    dropStep(n, xs)
  }

  def split[A](n: Int, xs: List[A]):(List[A], List[A]) = (n, xs) match {
    case (_, Nil) => (Nil, Nil)
    case (0, list) => (Nil, list)
    case (n, h :: tail) => {
      val (pre, post) = split(n - 1, tail)
      (h :: pre, post)
    }
  }

  def slice[A](start: Int, end: Int, xs: List[A]): List[A] = {
    (start, end, xs) match {
      case (_,_,Nil) => Nil
      case (0, end, xs) => xs.head::slice(0, end-1, xs.tail)
      case (start, end, xs) => slice(start-1, end-1, xs.tail)
    }
  }

  def rotate[A](n: Int, xs: List[A]): List[A] = {
    (n, xs) match {
      case (0, ls) => ls
      case (i, ls) => {
        if (i > 0) rotate(i-1, ls.tail:+ls.head)
        else rotate(xs.length + i, xs)
      }
    }
  }

  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = (n, xs) match {
    case (0, ls) => (ls.tail, ls.head)
    case (i, ls) => {
      val (l, v) = removeAt(n-1, xs.tail)
      (xs.head::l, v)
    }
  }

  def insertAt[A](e: A, n: Int, xs: List[A]): List[A] = (e, n, xs) match {
    case (e, 0, ls) => e::ls
    case (e, i, ls) => {
      val t = insertAt(e, i-1, ls.tail)
      ls.head::t
    }
  }

  def range(s: Int, e: Int): List[Int] = (s,e) match {
    case (i, j) => {
      if (j < i) Nil
      else i::range(i+1, j)
    }
  }

  def randomSelect[A](i: Int, xs: List[A]): List[Int] = {
    throw new NotImplementedException("")
  }

  def lotto(n: Int, e: Int): List[Int] = {
    throw new NotImplementedException("")
  }

  def randomPermute[A](ls: List[A]): List[A] ={
    throw new NotImplementedException("")
  }

  def combinations[A](l: Int, ls: List[A]): List[A] ={
    throw new NotImplementedException("")
  }

}