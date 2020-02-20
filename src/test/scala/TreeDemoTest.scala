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
    it should "do in-order traversal for a larger tree containing integers" in {
         /*
          this encodes the following tree:
                  4
               /     \
              2       4
             / \     / \
            1   3   2   5
                   / \
                  1   3
          */
        assert(td.traverse(td.replace(myTree,5,myTree)) === List(1,2,3,4,1,2,3,4,5))
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
    it should "do in-order traversal for a large tree containing strings" in {
        /*
        this encodes the following tree:
                       "four"
                      /      \
                    "two"   "five"
                    /   \
                "four" "three"
                /      \
            "two"    "five"
            /   \
        "one"  "three" 
        */

        val td2 = new TreeDemo[String]
        import td2._
        val myStrTree = Node(Node(Leaf("one"),"two",Leaf("three")),"four",Leaf("five"))
        assert(td2.traverse(td2.replace(myStrTree, "one", myStrTree)) === List("one","two","three","four","five","two","three","four","five"))
    }
    it should "do in-order traversal for empty tree" in {
        assert(td.traverse(Empty) === Nil)
    }
    it should "do in-order traversal for a leaf" in {
        assert(td.traverse(Leaf(1)) === List(1))
    }
    

    "Remove" should "properly remove the left subtree in the integer example" in {
        assert(td.remove(myTree, 2) === Node(Empty,4,Leaf(5)))
    }
    it should "properly remove the right subtree" in {
      assert(td.remove(myTree,5) == Node(Node(Leaf(1),2,Leaf(3)),4,Empty))
    }
    it should "properly remove a left leaf" in {
      assert(td.remove(myTree,1) == Node(Node(Empty,2,Leaf(3)),4,Leaf(5)))
    }
    it should "properly remove a right leaf" in {
      assert(td.remove(myTree,3) == Node(Node(Leaf(1),2,Empty),4,Leaf(5)))
    }
    it should "properly remove the root" in {
      assert(td.remove(myTree,4) == Empty)
    }
    it should "properly remove a lone node" in {
      assert(td.remove(Node(Empty,1,Empty),1) == Empty)
    }



    "Replace" should "properly replace the left subtree in the integer example" in {
        assert(td.replace(myTree, 2, myTree) === Node(myTree,4,Leaf(5)))
    }
    it should "properly replace an empty root" in {
      assert(td.replace(Leaf(1), 1, Node(Leaf(1),2,Leaf(3))) == Node(Leaf(1),2,Leaf(3)))
    }
    it should "properly replace a full root" in {
      assert(td.replace(Node(Leaf(0),1,Leaf(2)), 1, Node(Leaf(1),2,Leaf(3))) == Node(Leaf(1),2,Leaf(3)))
    }
    it should "properly replace a left root" in {
      assert(td.replace(Node(Leaf(0),1,Empty), 1, Node(Leaf(1),2,Leaf(3))) == Node(Leaf(1),2,Leaf(3)))
    }
    it should "properly replace a right root" in {
      assert(td.replace(Node(Empty,1,Leaf(2)), 1, Node(Leaf(1),2,Leaf(3))) == Node(Leaf(1),2,Leaf(3)))
    }
    it should "properly replace low leaf" in {
      assert(td.replace(Node(Node(Leaf(0),1,Leaf(2)),3,Leaf(4)), 0, Node(Leaf(-2),-1,Leaf(0))) === 
Node(Node(Node(Leaf(-2),-1,Leaf(0)),1,Leaf(2)),3,Leaf(4)), 0)
    }


    "GetMax" should "properly return the largest element of a small integer example" in {
        assert(td.getMax(Node(Empty,4,Leaf(5))) === Some(5))
    }
    it should "give the correct element for a larger integer example" in {
        assert(td.getMax(Node(Node(Leaf(1),2,Leaf(3)),4,Leaf(5))) === Some(5))
    }
    it should "give the correct element for an empty tree example" in {
        assert(td.getMax(Empty) === None)
    }
    it should "give the correct element for an small string example" in {
        val td2 = new TreeDemo[String]
        import td2._
        assert(td2.getMax(Node(Empty,"a",Leaf("b"))) === Some("b"))
    }
    it should "give the correct element for an non bst string example" in {
        val td2 = new TreeDemo[String]
        import td2._
         /*
        this encodes the following tree:
               "c"
             /     \
            "r"    "b"
            / \    /  \
          "q" "g" "z" "e"
                \
                "h"
        */
        assert(td2.getMax(Node(Node(Leaf("q"),"r",Node(Empty,"g",Leaf("h"))),"c",Node(Leaf("z"),"b",Leaf("e")))) === Some("z"))
    }
    it should "give the correct element for an leaf example" in {
        assert(td.getMin(Leaf(4)) === Some(4))
    }

    "GetMin" should "properly return the smallest element of a small integer example" in {
        assert(td.getMin(Node(Empty,4,Leaf(5))) === Some(4))
    }
    it should "give the correct element for a larger integer example" in {
        assert(td.getMin(Node(Node(Leaf(1),2,Leaf(3)),4,Leaf(5))) === Some(1))
    }
    it should "give the correct element for an empty tree example" in {
        assert(td.getMin(Empty) === None)
    }
    it should "give the correct element for an small string example" in {
        val td2 = new TreeDemo[String]
        import td2._
        assert(td2.getMin(Node(Empty,"a",Leaf("b"))) === Some("a"))
    }
    it should "give the correct element for an non bst string example" in {
        val td2 = new TreeDemo[String]
        import td2._
         /*
        this encodes the following tree:
               "c"
             /     \
            "r"    "b"
            / \    /  \
          "q" "g" "z" "e"
                \
                "h"
        */
        assert(td2.getMin(Node(Node(Leaf("q"),"r",Node(Empty,"g",Leaf("h"))),"c",Node(Leaf("z"),"b",Leaf("e")))) === Some("b"))
    }
    it should "give the correct element for an leaf example" in {
        assert(td.getMin(Leaf(4)) === Some(4))
    }

    "Is BST" should "give the correct answer for integer example" in {
        assert(td.isBST(myTree) === true)
    }
    it should "give the correct answer for simple non-BST" in {
        assert(td.isBST(td.replace(myTree, 3, Leaf(7))) === false)
    }
    it should "give correct answer for simple string example" in {
        val td2 = new TreeDemo[String]
        import td2._
         /*
        this encodes the following tree:
               "a"
                 \
                "b"
        */
        assert(td2.isBST(Node(Empty,"a",Leaf("b"))) === true)
    }
    it should "give correct answer for non-BST string example" in {
        val td2 = new TreeDemo[String]
        import td2._
         /*
        this encodes the following tree:
               "c"
             /     \
            "r"    "b"
            / \    /  \
          "q" "g" "z" "e"
                \
                "h"
        */
        assert(td2.isBST(Node(Node(Leaf("q"),"r",Node(Empty,"g",Leaf("h"))),"c",Node(Leaf("z"),"b",Leaf("e")))) === false)
    }
    it should "give correct answer for simple duplicate string example" in {
        val td2 = new TreeDemo[String]
        import td2._
         /*
        this encodes the following tree:
               "a"
                 \
                "a"
        */
        assert(td2.isBST(Node(Empty,"a",Leaf("a"))) === false)
          /*
        this encodes the following tree:
               "a"
              /
            "a"
        */
        assert(td2.isBST(Node(Leaf("a"),"a",Empty)) === false)
    }
    it should "give the correct element for an leaf example" in {
        assert(td.isBST(Leaf(4)) === true)
    }


    "BST Insert" should "give the correct answer for integer example" in {
        assert(td.insertBST(myTree,6) === td.replace(myTree,5,Node(Empty,5,Leaf(6))))
    } 
    it should "add correctly to Empty int tree" in {
      assert(td.insertBST(Empty,1) === Node(Empty,1,Empty))
    }
    it should "add correctly to tree when larger" in {
      assert(td.insertBST(Leaf(2), 3) === Node(Empty,2,Leaf(3)))
    }
    it should "add correctly to tree when smaller" in {
      assert(td.insertBST(Leaf(2), 1) === Node(Leaf(1),2,Empty))
    }
    it should "add correctly to tree leaf smaller" in {
      assert(td.insertBST(Node(Node(Leaf(2),3,Leaf(4)), 5,Leaf(6)), 0) === Node(Node(Node(Leaf(0),2,Empty),3,Leaf(4)),5,Leaf(6)))
    }
    it should "add correctly to tree leaf larger" in {
      assert(td.insertBST(Node(Node(Leaf(2),3,Leaf(4)), 5,Leaf(6)), 7) === Node(Node(Leaf(2),3,Leaf(4)),5,Node(Empty,6,Leaf(7))))
    }



    "BST Search" should "give the correct output for a string tree" in {
        val td2 = new TreeDemo[String]
        import td2._
     
        val myStringTree = Node(Node(Leaf("b"),"g",Node(Leaf("h"),"i",Leaf("j"))),"k",Leaf("l"))

         /*
          this encodes the following tree:
                  k
               /     \
              g       l
             / \    
            b   i
               / \ 
              h   j
          */
        assert(td2.searchBST(myStringTree,"a") === false)
        assert(td2.searchBST(myStringTree,"j") === true)
        assert(td2.searchBST(myStringTree,"g") === true)
        assert(td2.searchBST(myStringTree,"k") === true)
    }
    it should "give the correct answer for non-existant element" in {
        assert(td.searchBST(myTree,6) === false)
    }
    it should "give the correct answer for integer example Leaf" in {
        assert(td.searchBST(myTree,1) === true)
    }
    it should "give the correct answer for integer example Middle" in {
        assert(td.searchBST(myTree,2) === true)
    }
    it should "give the correct answer for integer example Root" in {
        assert(td.searchBST(myTree,4) === true)
    }
    it should "give the correct answer for char example leaf" in {
      val td2 = new TreeDemo[String]
      import td2._

      val myStringTree = Node(Node(Leaf("b"),"g",Node(Leaf("h"),"i",Leaf("j"))),"k",Leaf("l"))
      assert(td2.searchBST(myStringTree, "j") == true)
    }
}
