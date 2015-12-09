package com.github.johnynek.inliner

import org.scalacheck._
import Prop.forAll

import scala.util.{Failure, Success, Try}

object RangeTests extends Properties("Range") {

  import InlineRange._
  def rangeGen: Gen[Range] = for {
    start <- Gen.chooseNum(-1000, 1000)
    end <- Gen.chooseNum(start, 1000)
    step <- Gen.chooseNum(1, math.max(1, end - start))
  } yield new Range(start, end, step)

  implicit def rangeArb: Arbitrary[Range] = Arbitrary(rangeGen)

  property("inline.foreach works") = forAll { (l: Range) =>
    var res = 0
    l.inline.foreach { res += _ }

    res == l.sum
  }

  property("inline.foldLeft works") = forAll { (l: Range) =>
    l.inline.foldLeft(0)(_ + _) == l.sum
  }
  property("inline.foldLeft with case") = forAll { (l: Range) =>
    l.inline.foldLeft(Option.empty[Int]) {
      // This does not destructure into .isDefined calls (yet)
      case (None, arg) => Some(arg)
      case (Some(l), r) => Some(l + r)
    } == l.reduceOption(_ + _)
  }
  property("inline.reduceOption works") = forAll { (l: Range) =>
    l.inline.reduceOption(_ + _) == l.reduceOption(_ + _)
  }
  property("inline.find works") = forAll { (l: Range) =>
    l.inline.find(_ % 42 == 41) == l.find(_ % 42 == 41)
  }
  property("inline.forall works") = forAll { (l: Range) =>
    l.inline.forall(_ > 0) == l.forall(_ > 0)
  }
}
