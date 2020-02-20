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

  // TODO: replace all nodes equal to x in tree t with subtree
  def replace(t : BinaryTree, x : T, s : BinaryTree) : BinaryTree = {
    t match {
      case Empty => Empty
      case Node(l, d, r) => {
        if (d == x){
          s
        } else {
          Node(replace(l, x, s), d, replace(r, x, s))
        }
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
    // Check if the nodes of the tree are smaller on the left and greater or equal on the right
    def h(t: BinaryTree, min:Option[T], max:Option[T]) : Boolean = {
      t match {
        case Empty => true
        case Node(l, d, r) => {
          (min, max) match{
            case (None, None) => (h(l,None,Some(d)) && h(r,Some(d), None)) // Max and min are anything (head)
            case (None, Some(_max)) => (d <  _max && h(l, None, Some(d)) && h(r, Some(d), Some(_max))) // max is a num, min is anything (left traversal)
            case (Some(_min), None) => (d > _min && h(l, Some(_min), Some(d)) && h(r, Some(d), None)) // max is anything, min is num (right traversal)
            case (Some(_min), Some(_max)) => (d > _min && d < _max && h(l, Some(_min), Some(d)) && h(r, Some(d), Some(_max)))
          }
        }
      }
    }
    h(t, None, None)
  }


  // TODO: insert element x into binary search tree t
  def insertBST(t : BinaryTree, x : T) : BinaryTree = {
    t match {
      case Node(l,d,r) => {
        if(x > d){
          if(r == Empty) Node(l,d,Leaf(x))
          else Node(l, d, insertBST(r,x))
        }
        else{
          if(l == Empty) Node(Leaf(x),d,r)
          else Node(insertBST(l,x), d, r)
        }
      }
      case Empty => t
    }
  }

  // TODO: checks if element x is contained in binary search tree t
  def searchBST(t : BinaryTree, x : T) : Boolean = {
    t match {
      case Node(l, d, r) => {
        if(d == x) true
        if(l == x) true
        if(r == x) true
        if(d > x) searchBST(l,x)
        searchBST(r,x)
      }
      case Empty => false
    }
  }
}
