package us.jkk.examples.mergeRanges

import scala.annotation.tailrec

object Main extends App {
  case class InclusiveRange(start:Int, end:Int) {
    require (end >= start)
  }

  @tailrec
  def mergeSortedRanges(input: List[InclusiveRange], output:List[InclusiveRange]=List.empty): List[InclusiveRange] = {
    (input, output) match {
      case (Nil, accum) =>
        accum

      case (head::tail, Nil) =>
        mergeSortedRanges(tail, List(head))

      case (head::tail, accum :+ current) if head.start > current.end =>
        mergeSortedRanges(tail, output :+ head)

      case (head::tail, accum :+ current) if head.end > current.end =>
        mergeSortedRanges(tail, accum :+ current.copy(end=head.end))

      case (head::tail, accum) =>
        mergeSortedRanges(tail, accum)
    }
  }

  def mergeRanges(input: Set[InclusiveRange]): Seq[InclusiveRange] = {
    if(input.isEmpty) {
      Seq.empty
    } else {
      val sorted = input.toArray.sortInPlaceBy(_.start)
      var current = sorted.head
      var output: collection.mutable.ArrayBuffer[InclusiveRange] = collection.mutable.ArrayBuffer.empty
      for {
        next <- sorted.tail
      } {
        if(next.start > current.end) {
          //         [-current------]
          //                          [-next----]
          // ------------------------------------
          // yields                   [ current ]
          output.addOne(current)
          current = next
        } else if (next.end > current.end) {
          //          [-current------]
          //          [-next-------------]
          //   OR           [-next-------]
          // -------------------------------
          // yields   [ current          ]
          current = InclusiveRange(current.start, next.end)
        } else {
          //          [ current            ]
          //                [ next     ]
          // -------------------------------
          // yields   [ current            ]
        }
      }
      output.addOne(current)
      output.toSeq
    }
  }

  def test1() = {
    val testrange1 = Set(
      InclusiveRange(0, 10),
      InclusiveRange(2, 5),
      InclusiveRange(0, 5),
      InclusiveRange(5, 10),
      InclusiveRange(11, 20),
      InclusiveRange(15, 30)
    )
    println(mergeRanges(testrange1))
  }


  test1()






}
