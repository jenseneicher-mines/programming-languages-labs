import org.scalatest.FlatSpec

// unit tests for the List functions 
class ListDemoTest extends FlatSpec {
  val ld = new ListDemo[Int]

  // SELECTION SORT
  "Selection Sort" should "return a sorted list of five integers" in {
    assert(ld.selectionSort(List(3,2,4,1,5)) === List(1,2,3,4,5))
  }
  it should "return a sorted list of four integers" in {
    assert(ld.selectionSort(List(3,2,4,1)) === List(1,2,3,4))
  }
  it should "return a sorted list of six integers" in {
    assert(ld.selectionSort(List(12,2,6,10,7,9)) === List(2,6,7,9,10,12))
  }
  it should "return a sorted list of mixed sign integers" in {
    assert(ld.selectionSort(List(-9,3,6,4,1,10,27,14,12,2)) === List(-9,1,2,3,4,6,10,12,14,27))
  }
  it should "return a sorted list of sorted integers" in {
    assert(ld.selectionSort(List(1,2,3,4,5,6)) === List(1,2,3,4,5,6))
  }
  it should "return a sorted list of reverse sorted integers" in {
    assert(ld.selectionSort(List(7,6,5,4,3,2,1)) === List(1,2,3,4,5,6,7))
  }


  // MAP
  "Map" should "work properly with a basic +1 function" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>x+1) === List(2,3,4,5,6))
  }
  it should "work properly with a basic integer-to-string conversion" in {
    assert(ld.map(List(1,2,3,4,5),(x:Int)=>"$"+x) === List("$1","$2","$3","$4","$5"))
  }


  // FOLD LEFT
  "FoldLeft" should "sum the list elements properly" in {
    assert(ld.foldLeft(List(1,2,3),123,(x:Int,y:Int)=>x+y) === 6 + 123)
  }
  it should "sum the list elements properly #2" in {
    assert(ld.foldLeft(List(1,2,3,4),1234,(x:Int,y:Int)=>x+y) === 10 + 1234)
  }
  it should "sum the list elements properly #3" in {
    assert(ld.foldLeft(List(1,2,3,4,5),12345,(x:Int,y:Int)=>x+y) === 15 + 12345)
  }
  it should "sum the list elements properly #4" in {
    assert(ld.foldLeft(List(3,2,1),321,(x:Int,y:Int)=>x+y) === 6 + 321)
  }
  it should "sum the list elements properly #5" in {
    assert(ld.foldLeft(List(2,1,3,5),2135,(x:Int,y:Int)=>x+y) === 11 + 2135)
  }
  it should "sum the list elements properly #6" in {
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
  it should "properly capture integers less than a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x < 3)) === List(1,2))
  }
  it should "properly capture integers greater than or equal to a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x >= 3)) === List(4,5,3,6))
  }
  it should "properly capture integers not equal to a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x != 6)) === List(1,4,2,5,3))
  }
  it should "properly capture integers less than or equal to a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x <= 3)) === List(1,2,3))
  }
  it should "properly capture integers equal to a bound" in {
    assert(ld.filter(List(1,4,2,5,3,6),(x:Int)=>(x == 3)) === List(3))
  }


  // REVERSE
  "Reverse" should "properly reverse a list of integers" in {
    assert(ld.reverse(List(1,2,3,4)) === List(4,3,2,1))
  }
  it should "properly reverse a list of integers #2" in {
    assert(ld.reverse(List(1,2,3,4,5,6,7)) === List(7,6,5,4,3,2,1))
  }
  it should "properly reverse a list of integers #3" in {
    assert(ld.reverse(List(2,7,4,9,1)) === List(1,9,4,7,2))
  }
  it should "properly reverse a list of integers #4" in {
    assert(ld.reverse(List(2,10,6,3,2,1)) === List(1,2,3,6,10,2))
  }
  it should "properly reverse a list of integers with repeats" in {
    assert(ld.reverse(List(1,1,1,1,1,1,2,3,4,1,1,2)) === List(2,1,1,4,3,2,1,1,1,1,1,1))
  }
  it should "properly reverse a list of integers with one entry" in {
    assert(ld.reverse(List(1)) === List(1))
  }
}
