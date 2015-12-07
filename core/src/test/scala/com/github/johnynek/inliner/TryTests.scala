package com.github.johnynek.inliner

import org.scalacheck._
import Prop.forAll

import scala.util.{Failure, Success, Try}

object TryTests extends Properties("Try") {

  def successGen[T](g: Gen[T]): Gen[Success[T]] = g.map(Success(_))
  def failureGen[T](g: Gen[Throwable]): Gen[Failure[T]] = g.map(Failure[T](_))
  def tryGen[T](s: Gen[T], f: Gen[Throwable]): Gen[Try[T]] = Gen.oneOf(successGen(s), failureGen(f))

  implicit def arbTry[T](implicit arb: Arbitrary[T], err: Arbitrary[Throwable]): Arbitrary[Try[T]] =
    Arbitrary(tryGen(arb.arbitrary, err.arbitrary))

  import InlineTry._
  import Eqv._

  property("foreach") = forAll { (o: Try[Int]) =>
    var res: Option[Int] = None
    o.inline.foreach { i => res = Some(i) }
    res == o.toOption
  }


  property("filter") = forAll { (o: Try[Int]) =>
    eqv(o.inline.filter(_ % 2 == 0), o.filter(_ % 2 == 0))
  }

  property("for expressions work") = forAll { (o1: Int, o2: Int => Try[String], o3: String => Try[Long]) =>
    val correct = for {
      a <- Try { assert(o1 > 0); o1 }
      b <- o2(a)
      c <- o3(b)
    } yield c
    val fast = for {
      a <- inlineTry { assert(o1 > 0); o1 }.inline
      b <- o2(a).inline
      c <- o3(b).inline
    } yield c

    eqv(fast, correct)
  }

  property("flatten") = forAll { (o: Try[Try[Int]]) => o.inline.flatten == o.flatten }

  property("transform") = forAll { (o: Try[Int]) =>
    o.inline.fold(_.getMessage, _.toString) ==
      o.transform({i => Success(i.toString)}, {err => Success(err.getMessage)}).get
  }
  property("inlineTry") = forAll { (i: Int) =>
    eqv(inlineTry { assert(i < 0); i }, Try { assert(i < 0); i })
  }

  property("orElse") = forAll { (a: Try[Int], b: Try[Int]) =>
    eqv(a.inline.orElse(b), a.orElse(b))
  }
}
