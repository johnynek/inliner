package com.github.johnynek.inliner.benchmark.jmh

import com.github.johnynek.inliner.InlineTry._
import java.util.Random
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.util.{ Failure, Success, Try }
import org.openjdk.jmh.infra.Blackhole


object ConstException extends Exception("asdf")

object TryBenchmarks {
  private val ratioSuccessToFail = 100
}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.SingleShotTime))
@Warmup(batchSize = 4000)
@Measurement(batchSize = 10000)
@Fork(value = 1)
class TryBenchmarks {
  import TryBenchmarks._
  def rng: Random = new Random(42)

  @Benchmark
  def createTryInline(bh: Blackhole): Unit = {
    var iter = ratioSuccessToFail
    while (iter > 0) {
      iter -= 1
      bh.consume(inlineTry { if(iter % ratioSuccessToFail == 0) throw ConstException;
      iter
      })
    }

  }

  @Benchmark
  def createTry(bh: Blackhole): Unit = {
    var iter = ratioSuccessToFail
    while (iter > 0) {
      iter -= 1
      bh.consume(Try { if(iter % ratioSuccessToFail == 0) throw ConstException; iter })
    }
  }

  @Benchmark
  def createTryHandle(bh: Blackhole): Unit = {
    var iter = ratioSuccessToFail
    while (iter > 0) {
      iter -= 1
      val r = try {
        if(iter % ratioSuccessToFail == 0) throw ConstException;
        // Success("asdf")
        "asdf"
        } catch {
          case e: Throwable => ()// Failure(e)
      }
      bh.consume(r)
    }
  }

  @Benchmark
  def createTryHandleWithStackTrace(bh: Blackhole): Unit = {
    var iter = ratioSuccessToFail
    while (iter > 0) {
      iter -= 1
      val r = try {
        if(iter % ratioSuccessToFail == 0) sys.error("failed");
        Success("asdf")
        } catch {
          case e: Throwable => Failure(e)
      }
      bh.consume(r)
    }
  }

  @Benchmark
  def filterInline(): Unit = {
    var iter = 10000
    while (iter > 0) {
      iter -= 1
      Success(iter).inline.filter(_ % 10 != 0)
    }
  }
  @Benchmark
  def filter(): Unit = {
    var iter = 10000
    while (iter > 0) {
      iter -= 1
      Success(iter).filter(_ % 10 != 0)
    }
  }
  @Benchmark
  def flatMapInline(): Unit = {
    var iter = 10000
    while (iter > 0) {
      iter -= 1
      for {
        a <- inlineTry { assert(iter % 10 != 0); iter }.inline
        b <- inlineTry { assert(a * a % 11 != 0); a * a }.inline
      } yield b
    }
  }
  @Benchmark
  def flatMap(): Unit = {
    var iter = 10000
    while (iter > 0) {
      iter -= 1
      for {
        a <- Try { assert(iter % 10 != 0); iter }
        b <- Try { assert(a * a % 11 != 0); a * a }
      } yield b
    }
  }
}
