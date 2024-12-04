package utils

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class UnionFind[T](val elements: ArrayBuffer[T]) {
  outer =>

  def this(x: Seq[T]) = this(ArrayBuffer(x: _*))

  private case class Node(var parent: Option[Int], var treeSize: Int)

  private val nodes = ArrayBuffer.fill[Node](elements.size)(Node(None, 1))

  implicit class TOps(t1: T) {
    def <=>(t2: T): UnionFind[T] = outer.<=>(t1, t2)

    def <=>?(t2: T): Boolean = outer.<=>?(t1, t2)
  }

  def +=(x: T): UnionFind[T] =
    if (!elements.contains(x)) {
      elements += x
      nodes += Node(None, 1)
      this
    } else this

  def ++=(x: Seq[T]): UnionFind[T] = {
    x.foreach(this += _)
    this
  }

  def discard(x: T): UnionFind[T] =
    if (!elements.contains(x)) {
      println(s"[WARNING] Discarding unknown element $x")
      this
    } else {
      val xId = elements.indexOf(x)
      val toModify = for {n <- nodes
                          p <- n.parent if p == xId} yield n
      nodes(xId).parent match {
        case Some(parent) =>
          toModify.foreach(_.parent = Some(parent))
          nodes(parent).treeSize -= 1
        case None if toModify.nonEmpty =>
          toModify.tail.foreach(_.parent = Some(nodes.indexOf(toModify.head)))
          toModify.head.parent = None
          toModify.head.treeSize = nodes(xId).treeSize - 1
        case _ =>
      }
      for {
        n <- nodes
        p <- n.parent if p >= xId
      } n.parent = Some(p - 1)

      nodes.remove(xId)
      elements.remove(xId)
      this
    }

  /**
   * Provide the quotient set from the disjunction of this and that quotient sets that is a ~ b iff a ~ b in this or that
   * WARNING this is an in-place computation
   *
   * @param that the other quotient set
   * @return resulting quotient set
   */
  def ||(that: UnionFind[T]): UnionFind[T] = {
    val (keep, absorb) = if (elements.size >= that.elements.size) (this, that) else (that, this)
    val toAdd = absorb.elements -- keep.elements
    keep.elements ++= toAdd
    keep.nodes ++= ArrayBuffer.fill[keep.Node](toAdd.size)(keep.Node(None, 1))
    val toAddClasses = absorb.equivalenceClasses
    for {
      c <- toAddClasses if c.size >= 2
      l <- c.toSeq.sliding(2)
    }
      keep.<=>(l.head, l.last)
    keep
  }

  /**
   * Provide the quotient set from the conjonction of this and that quotient sets that is a ~ b iff a ~ b in this and that
   * WARNING this is an in-place computation
   *
   * @param that the other quotient set
   * @return resulting quotient set
   */
  def &&(that: UnionFind[T]): UnionFind[T] = {
    val (keep, absorb) = if (elements.size >= that.elements.size) (this, that) else (that, this)
    val toAdd = absorb.elements -- keep.elements
    keep.elements ++= toAdd
    keep.nodes ++= ArrayBuffer.fill[keep.Node](toAdd.size)(keep.Node(None, 1))
    val nonSingletonKeepClasses = keep.equivalenceClasses.filter(_.size >= 2)
    for {
      n <- keep.nodes if n.parent.isDefined || n.treeSize > 1
    } {
      n.parent = None
      n.treeSize = 1
    }
    for {
      keepClass <- nonSingletonKeepClasses
      absorbClass <- absorb.equivalenceClasses if absorbClass.size >= 2
      intersection = keepClass.intersect(absorbClass) if intersection.size >= 2
      l <- intersection.toSeq.sliding(2)
    }
      keep.<=>(l.head, l.last)
    keep
  }

  def addEquivalenceClass(x: Stream[T]): UnionFind[T] = {
    //    if(x.exists(e => !elements.contains(e))) return {
    //      println(s"[WARNING] ${x.filterNot(elements.contains).mkString(" and ")} not known element(s) of the union find")
    //      this
    //    }
    val ids = x.map(elements.indexOf)
    val roots = ids.map(root).distinct
    if (roots.size == 1) return this
    val maxRoot = roots.maxBy(i => nodes(i).treeSize)
    val sizeSum = roots.map(i => nodes(i).treeSize).sum
    for {
      r <- roots
    } {
      if (r == maxRoot)
        nodes(r).treeSize = sizeSum
      else
        nodes(r).parent = Some(maxRoot)
    }
    this
  }

  def <=>(t1: T, t2: T): UnionFind[T] = {
    if (!elements.contains(t1) || !elements.contains(t2)) return {
      println(s"[WARNING] ${List(t1, t2).filterNot(elements.contains).mkString(" and ")} not known element(s) of the union find")
      this
    }
    if (t1 == t2) return this
    val idT1 = elements.indexOf(t1)
    val idT2 = elements.indexOf(t2)
    val root1 = root(idT1)
    val root2 = root(idT2)
    if (root1 == root2) return this
    val node1 = nodes(root1)
    val node2 = nodes(root2)
    if (node1.treeSize < node2.treeSize) {
      node1.parent = Some(idT2)
      node2.treeSize += node1.treeSize
    } else {
      node2.parent = Some(idT1)
      node1.treeSize += node2.treeSize
    }
    this
  }

  def <=>?(t1: T, t2: T): Boolean = t1 == t2 || (
    elements.contains(t1) && elements.contains(t2) && root(elements.indexOf(t1)) == root(elements.indexOf(t2)))

  def equivalenceClasses: Set[Set[T]] =
    elements.indices.toSet.groupBy(i => root(i)).values.map(_.map(elements)).toSet

  def repr: Set[T] =
    elements.filter(e => nodes(elements.indexOf(e)).parent.isEmpty).toSet

  @tailrec
  private def root(t: Int): Int = nodes(t).parent match {
    case None => t
    case Some(p) => root(p)
  }
}

object UnionFind {
  def empty[T]: UnionFind[T] = new UnionFind[T](ArrayBuffer.empty)
}