package karazin.scala.users.group.week4.homework

import org.scalacheck.*
import Prop.{forAll, propBoolean}
import arbitraries.{empty, given}
import Homework.*

import scala.annotation.tailrec

object HomeworkSpecification extends Properties("Homework"):

  include(EmptySpecification)
  include(NonEmptySpecification)
  include(IntSetSpecification)

end HomeworkSpecification

// Add additional cases if needed
object EmptySpecification extends Properties("Empty"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[NonEmpty], given Arbitrary[IntSet]}

  property("equals to Empty") = propBoolean {
    Empty == Empty
  }

  property("not equal to NonEmpty") = forAll { (nonEmpty: NonEmpty) ⇒
    Empty != nonEmpty
  }

  property("include") = forAll { (element: Int) ⇒
    (Empty include element) == NonEmpty(element, Empty, Empty)
  }

  property("contains") = forAll { (element: Int) ⇒
    !(Empty contains element)
  }

  property("remove") = forAll { (element: Int) ⇒
    (Empty remove element) == Empty
  }

  property("union") = forAll { (set: IntSet) ⇒
    (Empty ∪ set) == set
  }

  property("intersection") = forAll { (set: IntSet) ⇒
    (Empty ∩ set) == Empty
  }

  property("complement of Empty") = forAll { (set: IntSet) ⇒
    (set ∖ Empty) == set
  }

  property("complement of set") = forAll { (set: IntSet) ⇒
    (Empty ∖ set) == Empty
  }

  property("left disjunctive union") = forAll { (set: IntSet) ⇒
    (Empty ∆ set) == set
  }

  property("right disjunctive union") = forAll { (set: IntSet) ⇒
    (set ∆ Empty) == set
  }

end EmptySpecification

// Add additional cases if needed
object NonEmptySpecification extends Properties("NonEmpty"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[NonEmpty], given Arbitrary[IntSet]}

  property("not equals to Empty") = forAll { (nonEmpty: NonEmpty) ⇒
    nonEmpty != Empty
  }

  property("equal") = forAll { (nonEmpty: NonEmpty) ⇒
    nonEmpty == nonEmpty
  }

  property("include") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    (nonEmpty include element) != nonEmpty
  }

  property("contains") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    if nonEmpty contains element
      then (nonEmpty include element) == NonEmpty
    else (nonEmpty include element) != NonEmpty
  }

  property("remove") = forAll { (nonEmpty: NonEmpty, element: Int) ⇒
    if nonEmpty contains element
      then (nonEmpty remove element) != nonEmpty
    else (nonEmpty remove element) == nonEmpty
  }

  property("union") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒

    @tailrec
    def resRec(acc: IntSet, maybeNonEmpty: IntSet): IntSet =
      if maybeNonEmpty.isInstanceOf[Empty]
        then acc
      // it's not look good 😅
      else resRec(acc include maybeNonEmpty.asInstanceOf[NonEmpty].elem, maybeNonEmpty remove maybeNonEmpty.asInstanceOf[NonEmpty].elem)

    (nonEmpty ∪ set) == resRec(set, nonEmpty)
  }

  property("intersection") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    @tailrec
    def resRec(acc: IntSet, maybeNonEmpty: IntSet, set: IntSet): IntSet =
      if maybeNonEmpty.isInstanceOf[Empty]
        then acc
      else if set contains maybeNonEmpty.asInstanceOf[NonEmpty].elem
        then resRec(acc include maybeNonEmpty.asInstanceOf[NonEmpty].elem, maybeNonEmpty remove maybeNonEmpty.asInstanceOf[NonEmpty].elem, set)
      else resRec(acc, maybeNonEmpty remove maybeNonEmpty.asInstanceOf[NonEmpty].elem, set)

    nonEmpty ∩ set == resRec(Empty, nonEmpty, set)
  }

  property("complement") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    @tailrec
    def resRec(acc: IntSet, maybeNonEmpty: IntSet, set: IntSet): IntSet =
      if maybeNonEmpty.isInstanceOf[Empty]
        then acc
      else if set contains maybeNonEmpty.asInstanceOf[NonEmpty].elem
        then resRec(acc, maybeNonEmpty remove maybeNonEmpty.asInstanceOf[NonEmpty].elem, set)
      else resRec(acc include maybeNonEmpty.asInstanceOf[NonEmpty].elem, maybeNonEmpty remove maybeNonEmpty.asInstanceOf[NonEmpty].elem, set)

    (nonEmpty ∖ set) == resRec(Empty, nonEmpty, nonEmpty ∩ set)
  }

  property("disjunctive") = forAll { (nonEmpty: NonEmpty, set: IntSet) ⇒
    (nonEmpty ∆ set) == (nonEmpty ∪ set) ∖ (nonEmpty ∩ set)
  }

end NonEmptySpecification

// Add additional cases if needed
object IntSetSpecification extends Properties("IntSet"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[IntSet]}

  property("equals") = forAll { (set: IntSet) ⇒
    set == set
  }

end IntSetSpecification