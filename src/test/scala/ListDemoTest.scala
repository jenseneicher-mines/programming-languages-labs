import org.scalatest.FlatSpec

// unit tests for the List functions 
class ListDemoTest extends FlatSpec {
  val ld = new ListDemo[Int]
  "Selection Sort" should "return a sorted list of five integers" in {
    assert(ld.selectionSort(List(3,2,4,1,5)) === List(1,2,3,4,5))
  }

  "Map" should "work properly with a basic +1 function" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>x+1) === List(2,3,4,5,6))
  }

  it should "work properly with a basic integer-to-string conversion" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>"$"+x) === List("$1","$2","$3","$4","$5"))
  }

  "FoldLeft" should "sum the list elements properly" in {
    assert(ld.foldLeft(List(1,2,3),123,(x:Int,y:Int)=>x+y) === 6 + 123)
  }

  "FoldRight" should "sum the list elements properly" in {
    assert(ld.foldRight(List(1,2,3),123,(y:Int,x:Int)=>y+x) === 6 + 123)
  }

  "Filter" should "properly capture integers greater than a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x > 3)) === List(4,5,6))
  }

  "Reverse" should "properly reverse a list of integers" in {
    assert(ld.reverse(List(1,2,3,4)) === List(4,3,2,1))
  }
}
