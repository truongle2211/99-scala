package logic {

  class S99Logic(a: Boolean) {
    import S99Logic._

    def and(b: Boolean) = (a, b) match {
      case (true, true) => true
      case _ => false
    }

    def or(b: Boolean) = (a, b) match {
      case (true, _) => true
      case (_, true) => true
      case _ => false
    }

    def equ(b: Boolean) = (a and b) or (not(a) and not(b))

    def nand(b: Boolean) = not(a and b)

    def nor(a: Boolean, b: Boolean) = not(a or b)

    def xor(a: Boolean, b: Boolean) = not(a equ  b)

    def impl(a: Boolean, b: Boolean) = a or not(b)
  }

  object S99Logic {

    implicit def boolean2S99Logic(a: Boolean): S99Logic = new S99Logic(a)

    def not(a: Boolean) = a match {
      case false => true
      case _ => false
    }

    def table2(f: (Boolean, Boolean) => Boolean) = {
      print("A    B    result")
      for {a <- List(true, false)
           b <- List(true, false)} {
        printf("%-5s %-5s %-5s", a, b, f(a, b))
      }
    }

    def Gray(n: Int): List[String] = n match {
      case 1 => List("0", "1")
      case _ => {
        val lower = Gray(n-1)
        lower flatMap (x => List(x+"0", x + "1"))
      }
    }

    def huffman[A](ls: List[(A, Int)]): List[(A,String)] = {
      throw new NotImplementedError("")
    }
  }

}
