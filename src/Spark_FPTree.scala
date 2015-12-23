/**
  * All of the code is from Spark mllib FPTree.scala
  * just for learn
  * Created by Takechiyo on 2015/12/22.
  */

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object FPTree{
  /**Representing a node in an FP-Tree*/
  class Node[T](val parent: Node[T]) extends serializable{
    var item: T = _
    var count: Long = 0L
    val children: mutable.Map[T, Node[T]] = mutable.Map.empty

    def isRoot: Boolean = parent == null
  }

  /**Summary of a item in an FP-Tree*/
  private class Summary[T] extends  serializable{
    var count: Long = 0L
    var nodes: ListBuffer[Node[T]] = ListBuffer.empty
  }
}

class FPTree[T] extends serializable{

  import FPTree._

  val root: Node[T] = new Node(null)

  private val summaries: mutable.Map[T, Summary[T]] = mutable.Map.empty

  /**Adds a transaction with count.*/
  def add(t: Iterator[T], count: Long = 1L): this.type = {
    require(count > 0)
    var curr = root
    curr.count += count
    t.foreach { item =>
      val summary = summaries.getOrElseUpdate(item, new Summary)
      summary.count += count
      val child = curr.children.getOrElseUpdate(item, {
        val newNode = new Node(curr)
        newNode.item = item
        summary.nodes += newNode
        newNode
      })
      child.count += count
      curr = child
    }
    this
  }

  /**Merges another FP-Tree. */
  def merge(other: FPTree[T]): this.type = {
    this
  }

  /**Gets a subtree with the suffix. */
  private def project(suffix : T): FPTree[T] = {
    val tree = new FPTree[T]
    if(summaries.contains(suffix)) {
      val summary = summaries(suffix)
      summary.nodes.foreach { node =>
        var t = List.empty[T]      // Represent a transaction (from this suffix to root)
        var curr = node.parent
        while (!curr.isRoot) {
          t = curr.item :: t
          curr = curr.parent
        }
        tree.add(t.iterator, node.count)
      }
    }
    tree
  }

  /** Returns all transactions in an iterator. */
  def transactions: Iterator[(List[T], Long)] = getTransactions(root)

  /** Returns all transactions under this node. */
  private def getTransactions(node: Node[T]): Iterator[(List[T], Long)] = {
    var count = node.count
    node.children.iterator.flatMap { case (item, child) =>
      getTransactions(child).map { case (t, c) =>
        count -= c
        (item :: t, c)
      }
    } ++ {
      if (count > 0) {
        Iterator.single((Nil, count))
      } else {
        Iterator.empty
      }
    }
  }

  /** Extracts all patterns with valid suffix and minimum count. */
  def extract (
              minCount: Long,
              validateSuffix: T => Boolean = _ => true
              ): Iterator[(List[T], Long)] = {
    summaries.iterator.flatMap { case (item, summary) =>
      if(validateSuffix(item) && summary.count >= minCount) {
        Iterator.single((item :: Nil, summary.count)) ++
          project(item).extract(minCount).map { case (t, c) =>
            (item :: t, c)
          }
      } else {
        Iterator.empty
      }
    }
  }

  /** Traverse the tree. */
  private def traverse(node: Node[T]): Unit = {
    node.children.foreach { case (item, child) =>
      println(item)
      traverse(child)
    }
  }

  def show(): Unit = traverse(root)

}

object Spark_FPTree {
  def main(args: Array[String]) {
    val tree = new FPTree[Char]
    val t1 = List('a', 'b', 'c')
    val t2 = List('b', 'c', 'f')
    tree.add(t1.iterator, 2)
    tree.add(t2.iterator, 3)
    tree.show()
    tree.transactions.foreach { case (t, c) =>
      println(t)
    }
    println(tree.root.count)
    println("hello world")
    System.out.println("hello world!")
  }
}
