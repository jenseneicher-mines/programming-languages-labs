import org.scalatest.FlatSpec

// unit tests for the List functions 
class ListDemoTest extends FlatSpec {
  val ld = new ListDemo[Int]

  // SELECTION SORT
  "Selection Sort" should "return a sorted list of five integers" in {
    assert(ld.selectionSort(List(3,2,4,1,5)) === List(1,2,3,4,5))
  }

  // MAP +1
  "Map" should "work properly with a basic +1 function" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>x+1) === List(2,3,4,5,6))
  }

  // MAP integer string

  it should "work properly with a basic integer-to-string conversion" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>"$"+x) === List("$1","$2","$3","$4","$5"))
  }


  // FOLD LEFT
  "FoldLeft" should "sum the list elements properly" in {
    assert(ld.foldLeft(List(1,2,3),123,(x:Int,y:Int)=>x+y) === 6 + 123)
  }
  "FoldLeft" should "sum the list elements properly #2" in {
    assert(ld.foldLeft(List(1,2,3,4),1234,(x:Int,y:Int)=>x+y) === 10 + 1234)
  }
  "FoldLeft" should "sum the list elements properly #3" in {
    assert(ld.foldLeft(List(1,2,3,4,5),12345,(x:Int,y:Int)=>x+y) === 15 + 12345)
  }
  "FoldLeft" should "sum the list elements properly #4" in {
    assert(ld.foldLeft(List(3,2,1),321,(x:Int,y:Int)=>x+y) === 6 + 321)
  }
  "FoldLeft" should "sum the list elements properly #5" in {
    assert(ld.foldLeft(List(2,1,3,5),2135,(x:Int,y:Int)=>x+y) === 11 + 2135)
  }
  "FoldLeft" should "sum the list elements properly #6" in {
    assert(ld.foldLeft(List(1,2,3,4,5,6),123456,(x:Int,y:Int)=>x+y) === 21 + 123456)
  }


  // FOLD RIGHT
  "FoldRight" should "sum the list elements properly" in {
    assert(ld.foldRight(List(1,2,3),123,(y:Int,x:Int)=>y+x) === 6 + 123)
  }

  // FILTER
  "Filter" should "properly capture integers greater than a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x > 3)) === List(4,5,6))
  }

  // REVERSE
  "Reverse" should "properly reverse a list of integers" in {
    assert(ld.reverse(List(1,2,3,4)) === List(4,3,2,1))
  }
}
