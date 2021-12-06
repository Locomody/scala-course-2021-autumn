package karazin.scala.users.group.week3

import scala.math.*
import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import Homework.*
import karazin.scala.users.group.week3.arbitraries.{nat, zero}

object HomeworkSpecification extends Properties("Homework"):

  include(ZeroSpecification)
  include(SuccSpecification)

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):
  import arbitraries.{given Arbitrary[Zero], given Arbitrary[Nat]}

  property("IsZeroCheck") = forAll { (zero: Zero) =>
    zero.isZero
  }

  property("predecessor throws exception") = forAll { (zero: Zero) =>
    throws(classOf[Exception]) {
      zero.predecessor
    }
  }

  property("zero addition") = forAll { (zero: Zero, nat: Nat) =>
    zero + nat == nat
  }

  property("zero equality") = forAll { (zero: Zero, nat: Nat) =>
    if nat.isZero
    then zero == nat
    else !(zero == nat)
  }

  property("zero subtraction") = forAll { (zero: Zero, nat: Nat) =>
    if nat.isZero
    then zero - nat == Zero
    else throws(classOf[IllegalArgumentException]) {
      zero - nat
    }
  }

  property("zero toInt") = forAll {(zero: Zero) =>
    zero.toInt == 0
  }


end ZeroSpecification


object SuccSpecification extends Properties("Succ"):
  import arbitraries.given Arbitrary[Succ]
  import arbitraries.given Arbitrary[Int]

  property("Succ is zero") = forAll {(succ: Succ) =>
    !succ.isZero
  }

  property("Succ to int/from int") = forAll {(n: Int) =>
    val ints: Vector[Int] = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    Nat.fromInt(n).toInt == ints(n)
  }

  property("Succ ==") = forAll {(left: Succ, right: Succ ) =>
    (left == right) == (left.toInt == right.toInt)
  }

  property("Succ subtraction") = forAll {(left: Succ, right: Succ) =>
    if right.toInt <= left.toInt
    then (left - right).toInt == (left.toInt - right.toInt)
    else throws(classOf[IllegalArgumentException]) {
      (left - right).toInt == (left.toInt - right.toInt)
    }
  }

  property("Succ addition") = forAll {(left: Succ, right: Succ) =>
    (left + right).toInt == (left.toInt + right.toInt)
  }

  property("Succ predecessor") = forAll { (succ: Succ) =>
    succ.predecessor == succ - Succ(Zero)
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
 
  // :-(((((

end NatSpecification
  