package Problems

import jdk.jshell.spi.ExecutionControl.NotImplementedException

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
    throw new NotImplementedException("")
  }
}