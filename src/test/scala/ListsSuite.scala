package Problems

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {

  import Problems.Lists._

  test("P01 -- last element of list") {
    assert(last(List(1,2,3)) == 3)
    assert(last(List('x')) == 'x')
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

}
