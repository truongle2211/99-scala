package logic {
  object S99Logic {
    def not(a: Boolean) = a match {
      case false => true
      case _ => false
    }

    def and(a: Boolean, b: Boolean) = (a,b) match {
      case (true, _) => true
      case (_, true) => true
      case _ => false
    }

    def or(a: Boolean, b: Boolean) = (a, b) match {
      case (false, _) => false
      case (_, false) => false
      case _ => true
    }

    def nand(a: Boolean, b: Boolean) = not(and(a,b))
    def nor(a: Boolean, b: Boolean) = not(or(a,b))
    def equ(a: Boolean, b: Boolean) = or(and(a,b), and(not(a), not(b)))
    def xor(a: Boolean, b: Boolean) = not(equ(a,b))
    def impl(a: Boolean, b: Boolean) = or(a, not(b))

  }
}
