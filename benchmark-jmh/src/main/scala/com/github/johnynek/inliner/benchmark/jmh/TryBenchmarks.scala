package com.github.johnynek.inliner.benchmark.jmh

import com.github.johnynek.inliner.InlineTry._
import java.util.Random
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.util.{Failure, Success, Try}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class TryBenchmarks {
  def rng: Random = new Random(42)

  @Benchmark
  def createTryInline(): Unit = {
    var iter = 10000
    while(iter > 0) {
      iter -= 1
      inlineTry { assert(iter % 10 != 0); iter }
    }
  }
  @Benchmark
  def createTry(): Unit = {
    var iter = 10000
    while(iter > 0) {
      iter -= 1
      Try { assert(iter % 10 != 0); iter }
    }
  }
  @Benchmark
  def filterInline(): Unit = {
    var iter = 10000
    while(iter > 0) {
      iter -= 1
      Success(iter).inline.filter(_ % 10 != 0)
    }
  }
  @Benchmark
  def filter(): Unit = {
    var iter = 10000
    while(iter > 0) {
      iter -= 1
      Success(iter).filter(_ % 10 != 0)
    }
  }
  @Benchmark
  def flatMapInline(): Unit = {
    var iter = 10000
    while(iter > 0) {
      iter -= 1
      for {
        a <- inlineTry { assert(iter % 10 != 0); iter }.inline
        b <- inlineTry { assert(a*a % 11 != 0); a*a }.inline
      } yield b
    }
  }
  @Benchmark
  def flatMap(): Unit = {
    var iter = 10000
    while(iter > 0) {
      iter -= 1
      for {
        a <- Try { assert(iter % 10 != 0); iter }
        b <- Try { assert(a*a % 11 != 0); a*a }
      } yield b
    }
  }
}
