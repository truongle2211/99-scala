package arithmetic {

  class S99Int(val start: Int) {

    import S99Int._

    def isPrime: Boolean = (start > 1) && (primes takeWhile {
      _ <= Math.sqrt(start)
    } forall {
      start % _ != 0
    })

    def isCoprimeTo(m: Int): Boolean = gcd(start, m) == 1

    def totient: Int = (1 to start) count { start.isCoprimeTo(_)}

    def primeFactors: List[Int] = {
      throw new NotImplementedError("")
    }

    def primeFactorMultiplicity: List[(Int, Int)] ={
      throw new NotImplementedError("")
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // create a stream of primes
    var primes = Stream.cons(2, Stream.from(3, 2) filter {
      _.isPrime
    })

    def gcd(n1: Int, n2: Int): Int = (n1, n2) match {
      case (0, n) => n
      case (n, m) => gcd(m%n, n)
    }

    def listPrimesinRange(r: Range): List[Int] = primes dropWhile(_ > r.start) takeWhile(_ < r.last) toList
  }

}
