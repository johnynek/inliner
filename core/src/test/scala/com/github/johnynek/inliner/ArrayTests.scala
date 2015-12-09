package com.github.johnynek.inliner

import org.scalacheck._
import Prop.forAll

import scala.util.{Failure, Success, Try}

object ArrayTests extends Properties("Array") {

  import InlineArray._

  property("Array.inline.foreach works") = forAll { (l: Array[String]) =>
    var res = ""
    l.inline.foreach { res += _ }

    res == l.mkString
  }
  property("Array.inline.foreach[Int] works") = forAll { (l: Array[Int]) =>
    var res = 0
    l.inline.foreach { res += _ }

    res == l.sum
  }

  property("Array.inline.foldLeft works") = forAll { (l: Array[String]) =>
    l.inline.foldLeft("")(_ + _) == l.mkString
  }
  property("Array.inline.foldLeft[Int] works") = forAll { (l: Array[Int]) =>
    l.inline.foldLeft(0)(_ + _) == l.sum
  }
  property("Array.inline.foldLeft with case") = forAll { (l: Array[String]) =>
    l.inline.foldLeft(Option.empty[String]) {
      // This does not destructure into .isDefined calls (yet)
      case (None, arg) => Some(arg)
      case (Some(l), r) => Some(l + r)
    } == l.reduceOption(_ + _)
  }
  property("Array.inline.reduceOption works") = forAll { (l: Array[String]) =>
    l.inline.reduceOption(_ + _) == l.reduceOption(_ + _)
  }

  property("Array.inline.find works") = forAll { (l: Array[Int]) =>
    l.inline.find(_ % 42 == 41) == l.find(_ % 42 == 41)
  }

  property("Array.inline.forall works") = forAll { (l: Array[Int]) =>
    l.inline.forall(_ > 0) == l.forall(_ > 0)
  }
}
