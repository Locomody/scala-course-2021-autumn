package karazin.scala.users.group.week2.homework

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import utils._

object HomeworkSpecification extends Properties("Homework"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[Rational]}

  val KINDA_SMALL_NUMBER = 0.0000000001

  property("throw exception due to zero denominator") = forAll { (numer: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, -abs(kindaDenom))
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Int, int: Int) ⇒
    val denom = abs(int) + 1
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == ( left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == ( left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    -rational == Rational(-rational.numer, rational.denom)
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    abs((left + right).toDouble - (left.toDouble + right.toDouble)) <= KINDA_SMALL_NUMBER
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    abs((left - right).toDouble - (left.toDouble - right.toDouble)) <= KINDA_SMALL_NUMBER
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    abs((left * right).toDouble - (left.toDouble * right.toDouble)) <= KINDA_SMALL_NUMBER
  }

  property("division") = forAll { (left: Rational, numer: Int, denom: Int) =>
    val right = Rational(if numer == 0 then 1 else numer, abs(denom) + 1)
    abs((left / right).toDouble - (left.toDouble / right.toDouble)) <= KINDA_SMALL_NUMBER
  }

  property("division by zero") = forAll { (left: Rational, int: Int) =>
    throws(classOf[IllegalArgumentException]) {
      left / Rational(0, int)
    }
  }

end HomeworkSpecification