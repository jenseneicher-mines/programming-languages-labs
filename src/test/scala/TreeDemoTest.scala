import org.scalatest.FlatSpec

// unit tests for the Tree functions 
class TreeDemoTest extends FlatSpec {
    val td = new TreeDemo[Int]
    import td._
    /*
    this encodes the following tree:
      4
     / \
    2   5
    / \
   1   3
    */

    val myTree = Node(Node(Leaf(1),2,Leaf(3)),4,Leaf(5))
    
    "Traverse" should "do in-order traversal for a small tree containing integers" in {
        assert(td.traverse(myTree) === List(1,2,3,4,5))
    }

    it should "do in-order traversal for a small tree containing strings" in {
        /*
        this encodes the following tree:
          "four"
         /      \
        "two"   "five"
        /   \
     "one"  "three"
        */

        val td2 = new TreeDemo[String]
        import td2._
        val myStrTree = Node(Node(Leaf("one"),"two",Leaf("three")),"four",Leaf("five"))
        assert(td2.traverse(myStrTree) === List("one","two","three","four","five"))
    }

    "Remove" should "properly remove the left subtree in the integer example" in {
        assert(td.remove(myTree, 2) === Node(Empty,4,Leaf(5)))
    }

    "Replace" should "properly replace the left subtree in the integer example" in {
        assert(td.replace(myTree, 2, myTree) === Node(myTree,4,Leaf(5)))
    }

    "Is BST" should "give the correct answer for integer example" in {
        assert(td.isBST(myTree) === true)
    }

    it should "give the correct answer for simple non-BST" in {
        assert(td.isBST(td.replace(myTree, 3, Leaf(7))) === false)
    }

    "BST Insert" should "give the correct answer for integer example" in {
        assert(td.insertBST(myTree,6) === td.replace(myTree,5,Node(Empty,5,Leaf(6))))
    }

    "BST Search" should "give the correct answer for non-existant element" in {
        assert(td.searchBST(myTree,6) === false)
    }

    it should "give the correct answer for integer example" in {
        assert(td.searchBST(myTree,2) === true)
    }
}
