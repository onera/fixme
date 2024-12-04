package operators

import operators.Transformable._


trait Streamable[T] {
  /**
   * Filter a set of indices k and perform a shift of the date within a TFlow
   *
   * @param k  the indices to keep
   * @param of the size of the shift
   * @param f  the TFlow to shift
   * @return the shifted TFlow
   */
  def shift(k: Array[Int], of: Int, f: TFlow[T]): TFlow[T]

  /**
   * Perform a merge between two TFlows preserving the order
   *
   * @param l left TFlow
   * @param r right TFlow
   * @return the merged TFlow
   */
  def merge(l: TFlow[T], r: TFlow[T]): TFlow[T]

  def genValue(i: Int): T

  def toDates(l:TFlow[T]): TFlow[Int]

  def toData(l:TFlow[T]): TFlow[Int]

  /**
   * Merge of the flows of shift registers
   *
   * @param id             the container's name of the shift registers
   * @param shiftRegisters the shift registers
   * @param indexMap       the correspondance between register indices used to perform the merge
   * @return a series of registers resulting from the merging of the flows
   */
  def merge(id: String, shiftRegisters: Array[ShiftRegisterUnit[T]], indexMap: Map[(Int, Int), Int]): Array[SimpleRegister[T]] = (for {
    h <- shiftRegisters.headOption
  } yield {
    val outputFlows = shiftRegisters
      .zipWithIndex
      .flatMap(p => p._1.registers.zipWithIndex.map(ri => ri.copy(_2 = indexMap(p._2, ri._2))))
      .groupBy(_._2)
      .toSeq
      .sortBy(_._1)
      .map(p => Flow(s"$id.r${p._1}.o", p._2.map(_._1.output.tFlow).reduce[TFlow[T]](merge)))

    (0 until shiftRegisters.map(_.registers.length).max).toArray.map(
      i => SimpleRegister(s"$id.r$i", if (i == 0) h.inputTFlow else outputFlows(i - 1).tFlow, outputFlows(i)))
  }) getOrElse Array.empty


  /**
   * Merge of the flows of shift registers
   *
   * @param id             the container's name of the shift registers
   * @param shiftRegisters the shift registers
   * @return a series of registers resulting from the merging of the flows
   */
  def merge(id: String, shiftRegisters: Array[ShiftRegisterUnit[T]]): Array[SimpleRegister[T]] = {
    val mapping = (for {
      s <- shiftRegisters.indices
      r <- shiftRegisters(s).registers.indices
    } yield {
      (s, r) -> r
    }).toMap
    merge(id, shiftRegisters, mapping)
  }
}

object Streamable {

  trait Instances {
    implicit class FlowLikeOps[T](x: TFlow[T])(implicit ev: Streamable[T]) {
      def shift(k: Array[Int], of: Int): TFlow[T] = ev.shift(k, of, x)

      def shift(of: Int): TFlow[T] = ev.shift(x.indices.toArray, of, x)

      def merge(r: TFlow[T]): TFlow[T] = ev.merge(x, r)

      def toDates: TFlow[Int] = ev.toDates(x)

      def toData: TFlow[Int] = ev.toData(x)
    }

    case class Element(date: Int, dataId: Int) extends Ordered[Element] {
      def compare(that: Element): Int = date.compare(that.date)

      override def toString: String = s"d_$dataId@$date"
    }

    case class CorruptibleElement(date: Int, dataId: Int, corrupted: Boolean = false) extends Ordered[CorruptibleElement] {
      def compare(that: CorruptibleElement): Int = date.compare(that.date)

      override def toString: String = s"d_$dataId${if (corrupted) "*" else ""}@$date"
    }

    implicit val IntIsFlowLike: Streamable[Int] = new Streamable[Int] {

      def shift(k: Array[Int], of: Int, f: TFlow[Int]): TFlow[Int] = f.zipWithIndex.collect {
        case (_, i) if k.contains(i) => f.lift(i + of) getOrElse (f.last + (i + of + 1 - f.size))
      }

      def merge(l: TFlow[Int], r: TFlow[Int]): TFlow[Int] = {
        if (l.nonEmpty && r.nonEmpty) {
          if (l.head == r.head) {
            l.head #:: merge(l.tail, r.tail)
          } else if (l.head < r.head) {
            l.head #:: merge(l.tail, r)
          } else {
            r.head #:: merge(l, r.tail)
          }
        } else if (r.nonEmpty)
          r
        else
          l
      }

      def genValue(i: Int): Int = i

      def toDates(l: TFlow[Int]): TFlow[Int] = l

      def toData(l: TFlow[Int]): TFlow[Int] = l.indices.toStream
    }


    implicit val dateDataIsFlowLike: Streamable[Element] = new Streamable[Element] {

      def shift(k: Array[Int], of: Int, f: TFlow[Element]): TFlow[Element] = f.zipWithIndex.collect {
        case (Element(_, d), i) if k.contains(i) =>
          val date = f.lift(i + of).map(_.date) getOrElse f.last.date + (i + of + 1 - f.size)
          Element(date, d)
      }

      def merge(l: TFlow[Element], r: TFlow[Element]): TFlow[Element] = {
        if (l.nonEmpty && r.nonEmpty) {
          if (l.head.date == r.head.date) {
            l.head #:: merge(l.tail, r.tail) //WARNING ASSUME that l.head.dataId == r.head.dataId
          } else if (l.head.date < r.head.date) {
            l.head #:: merge(l.tail, r)
          } else {
            r.head #:: merge(l, r.tail)
          }
        } else if (r.nonEmpty)
          r
        else
          l
      }

      def genValue(i: Int): Element = Element(i, i)

      def toDates(l: TFlow[Element]): TFlow[Int] = l.map(_.date)

      def toData(l: TFlow[Element]): TFlow[Int] = l.map(_.dataId)
    }

    implicit val corruptibleElementIsFlowLike: Streamable[CorruptibleElement] = new Streamable[CorruptibleElement] {

      def shift(k: Array[Int], of: Int, f: TFlow[CorruptibleElement]): TFlow[CorruptibleElement] = f.zipWithIndex.collect {
        case (CorruptibleElement(_, d, c), i) if k.contains(i) =>
          val date = f.lift(i + of).map(_.date) getOrElse f.last.date + (i + of + 1 - f.size)
          CorruptibleElement(date, d, c)
      }

      /**
       * Perform a merge between two TFlows preserving the order
       *
       * @param l left TFlow
       * @param r right TFlow
       * @return the merged TFlow
       */
      def merge(l: TFlow[CorruptibleElement], r: TFlow[CorruptibleElement]): TFlow[CorruptibleElement] = {
        if (l.nonEmpty && r.nonEmpty) {
          if (l.head.date == r.head.date) {
            l.head.copy(corrupted = l.head.corrupted || r.head.corrupted) #:: merge(l.tail, r.tail) //WARNING ASSUME that l.head.dataId == r.head.dataId
          } else if (l.head.date < r.head.date) {
            l.head #:: merge(l.tail, r)
          } else {
            r.head #:: merge(l, r.tail)
          }
        } else if (r.nonEmpty)
          r
        else
          l
      }

      def genValue(i: Int): CorruptibleElement = CorruptibleElement(i, i)

      def toDates(l: TFlow[CorruptibleElement]): TFlow[Int] = l.map(_.date)

      def toData(l: TFlow[CorruptibleElement]): TFlow[Int] = l.map(_.dataId)
    }
  }
}