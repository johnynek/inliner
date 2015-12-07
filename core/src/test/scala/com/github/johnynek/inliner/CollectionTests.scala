package com.github.johnynek.inliner

import org.scalacheck._
import Prop.forAll

import scala.util.{Failure, Success, Try}

object CollectionTests extends Properties("Collections") {

  import InlineCollection._

  property("List.inline.foreach works") = forAll { (l: List[String]) =>
    var res = ""
    l.inline.foreach { res += _ }

    res == l.mkString
  }

  property("List.inline.foldLeft works") = forAll { (l: List[String]) =>
    l.inline.foldLeft("")(_ + _) == l.mkString
  }
  property("List.inline.foldLeft with case") = forAll { (l: List[String]) =>
    l.inline.foldLeft(Option.empty[String]) {
      // This does not destructure into .isDefined calls (yet)
      case (None, arg) => Some(arg)
      case (Some(l), r) => Some(l + r)
    } == l.reduceOption(_ + _)
  }
  property("List.inline.reduceOption works") = forAll { (l: List[String]) =>
    l.inline.reduceOption(_ + _) == l.reduceOption(_ + _)
  }

  property("List.inline.find works") = forAll { (l: List[Int]) =>
    l.inline.find(_ % 42 == 41) == l.find(_ % 42 == 41)
  }

  property("List.inline.forall works") = forAll { (l: List[Int]) =>
    l.inline.forall(_ > 0) == l.forall(_ > 0)
  }
}
