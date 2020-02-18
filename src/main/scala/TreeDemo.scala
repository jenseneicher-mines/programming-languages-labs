class TreeDemo[T](implicit o : T => Ordered[T]) {
  // NOTE - Leaf(x) is now represented as Node(Empty,x,Empty)
  sealed trait BinaryTree
  case object Empty extends BinaryTree
  case class Node(left:BinaryTree, d:T, right:BinaryTree) extends BinaryTree
  //case class Leaf(d:T) extends BinaryTree

  // construct a "leaf" node
  def Leaf(d : T) : BinaryTree = Node(Empty,d,Empty)

  // in-order traversal of binary tree
  // Unit type for functions that don't return anything
  // returns the proper order for the traversal (as a list)
  def traverse(t : BinaryTree) : List[T] = t match {
    case Empty => Nil
    case Node(l, d, r) => traverse(l) ++ List(d) ++ traverse(r)
  }

  // remove all nodes equal to x from tree t
  def remove(t : BinaryTree, x : T) : BinaryTree = {
    replace(t, x, Empty)
  }

  // TODO: replace all nodes equal to x in tree t with subtree s
  def replace(t : BinaryTree, x : T, s : BinaryTree) : BinaryTree = {
    t match {
      case Empty => Empty
      case Node(l, d, r) => {
        // TODO
        Empty
      }
    }
  }

  // get the maximum of two elements
  def max(x : T, y : Option[T]) : T = {
    y match {
      case None => x
      case Some(z) => if(z > x) z else x
    }
  }

  // get the minimum of two elements
  def min(x : T, y : Option[T]) : T = {
    y match {
      case None => x
      case Some(z) => if(z < x) z else x
    }
  }

  // get the maximum element in the tree
  def getMax(t : BinaryTree) : Option[T] = {
    t match {
      case Empty => None
      case Node(l, d, r) => Some(max(max(d, getMax(l)), getMax(r)))
    }
  }

  // get the minimum element in the tree
  def getMin(t : BinaryTree) : Option[T] = {
    t match {
      case Empty => None
      case Node(l, d, r) => Some(min(min(d, getMin(l)), getMin(r)))
    }
  }

  // TODO: check if t is a binary search tree
  def isBST(t : BinaryTree) : Boolean = {
    t match {
      case Empty => true
      case Node(l, d, r) => {
        // TODO
        false
      }
    }
  }


  // TODO: insert element x into binary search tree t
  def insertBST(t : BinaryTree, x : T) : BinaryTree = {
    t match {
      case Empty => Leaf(x)
      case Node(l,d,r) => {
        // TODO
        Empty
      }
    }
  }

  // TODO: checks if element x is contained in binary search tree t
  def searchBST(t : BinaryTree, x : T) : Boolean = {
    // TODO
    false
  }
}
