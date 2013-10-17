package tests.tca

import ca.uwaterloo.scalacg.annotation.target

object Sealed1 {

  sealed abstract class Tree
  case class Node(left: Tree, right: Tree) extends Tree
  case class Leaf[T](value: T) extends Tree
  case object Empty extends Tree

  @target("dps") def dps(t: Tree): Unit = t match {
    case Node(left, right) =>
      { "dps"; this}.dps(left); { "dps"; this}.dps(right)
    case Leaf(x) => println("Leaf " + x)
    case Empty => println("Empty") // Compiler warns if this line is omitted
  }
  
  def main(args : Array[String]){
    val n1 : Tree = Empty;
    val n2 : Tree = Leaf("hello");
    val n3 : Tree = Node(n1,n2);
    { "dps"; this}.dps(n3);
  }

}