package com.github.johnynek.inliner

import org.scalacheck._
import Prop.forAll

import scala.util.{Failure, Success, Try}

object OptionTests extends Properties("Option") {

  import InlineOption._
  import Eqv._

  property("fold literal") = forAll { (o: Option[Int], empty: String) =>
    o.inline.fold(empty)(_.toString) == o.fold(empty)(_.toString)
  }
  property("fold literal, capture") = forAll { (o: Option[Int], empty: String) =>
    val prefix = "yo"
    o.inline.fold(empty)(i => prefix + i) == o.fold(empty)(prefix + _)
  }

  property("foreach") = forAll { (o: Option[Int]) =>
    var res: Option[Int] = None
    o.inline.foreach { i => res = Some(i) }
    res == o
  }

  property("filter") = forAll { (o: Option[Int]) => o.inline.filter(_ % 2 == 0) == o.filter(_ % 2 == 0) }

  property("for expressions work") = forAll { (o1: Option[Int], o2: Int => Option[String], o3: String => Option[Long]) =>
    val correct = for {
      a <- o1
      b <- o2(a)
      c <- o3(b)
    } yield c
    val fast = for {
      a <- o1.inline
      b <- o2(a).inline
      c <- o3(b).inline
    } yield c

    fast == correct
  }

  property("flatten") = forAll { (o: Option[Option[Int]]) => o.inline.flatten == o.flatten }

  property("orElse") = forAll { (a: Option[Int], b: Option[Int]) =>
    eqv(a.inline.orElse(b), a.orElse(b))
  }
}
