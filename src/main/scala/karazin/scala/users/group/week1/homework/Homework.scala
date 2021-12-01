package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = if b then false else true

    def and(left: Boolean, right: => Boolean): Boolean = if left then right else false

    def or(left: Boolean, right: => Boolean): Boolean = if left then true else right

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (left, right) =>
    {

      @tailrec
      def multiplicationReq(left: BigInt, right: BigInt, acc :BigInt): BigInt =
        if right == 0 then acc
        else multiplicationReq(left, right - 1, acc + left)


      multiplicationReq(left, right, acc = 0)
    }

    val power: (BigInt, BigInt) => BigInt = (left, right) =>
    {

      @tailrec
      def powerReq(left: BigInt, right: BigInt, acc: BigInt): BigInt =
        if right == 0 then acc
        else powerReq(left, right - 1, multiplication(acc, left))

      powerReq(left, right, acc = 1)
    }

    val fermatNumber: Int => BigInt = (n:Int) => power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    // Why scala String != List[Char], so sad :-(

    val length: (List[Char]) => Int = (str) =>
    {
      @tailrec
      def lenghtRec(str: List[Char], acc: Int): Int =
        str match
        {
          case Nil => acc
          case _ :: tail => lenghtRec(tail, acc + 1)
        }

      lenghtRec(str, acc = 0)
    }

    val stringHead: (List[Char]) => Char = (list) =>
    {
      list match
      {
        case Nil => '\u0000'
        case head::_ => head
      }
    }

    val isStringEmpty: (List[Char]) => Boolean = (list) =>
    {
      list == Nil
    }

    // It's not optimized because of Strings immutability.
    // The more effective way to use StringBuilder maybe (???)
    val CharListToString: (List[Char]) => String = (list) =>
    {
      @tailrec
      def CharListToStringRec(list: List[Char], str: String): String =
        list match
        {
          case Nil => str
          case head :: tail => CharListToStringRec(tail, str ++ head.toString)
        }

      CharListToStringRec(list, str = "")
    }

    val splitStringIntoClusters: (List[Char]) => List[List[Char]] = (str) =>
    {
      @tailrec
      def splitStringIntoClustersRec(str: List[Char], clusters: List[List[Char]],
                                     cluster: List[Char]): List[List[Char]] =
        str match
        {
          case Nil => clusters :+ cluster
          case head :: tail =>
            (if (!isStringEmpty(tail)) && (head != stringHead(tail))
            then splitStringIntoClustersRec(tail, clusters :+ (cluster :+ head), List())
            else splitStringIntoClustersRec(tail, clusters, cluster :+ head))
        }

      splitStringIntoClustersRec(str, List(), List())
    }

    val lookAndSayNext: (List[Char]) => List[Char] = (str) =>
    {
      @tailrec
      def lookAndSayNextRec(sequence: List[List[Char]], nextSequence: List[Char]): List[Char] =
        sequence match
        {
          case Nil => nextSequence
          case head :: tail => lookAndSayNextRec(tail, nextSequence :+ (length(head) + 48).toChar :+ stringHead(head))
        }

      lookAndSayNextRec(splitStringIntoClusters(str), List())
    }

    val lookAndSaySequenceElement: Int => BigInt = (n: Int) =>
    {
      @tailrec
      def lookAndSaySequenceElementRec(element: List[Char], n: Int): List[Char] =
        if n == 0
        then element
        else lookAndSaySequenceElementRec(lookAndSayNext(element), n - 1)

      BigInt(CharListToString(lookAndSaySequenceElementRec(List('1'), n)))
    }

  end `Look-and-say Sequence`

end Homework