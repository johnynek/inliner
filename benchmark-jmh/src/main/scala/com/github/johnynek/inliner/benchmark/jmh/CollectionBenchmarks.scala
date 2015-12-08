package com.github.johnynek.inliner.benchmark.jmh

import com.github.johnynek.inliner.InlineCollection._
import java.util.Random
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.util.{Failure, Success, Try}

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class CollectionBenchmarks {
  def rng: Random = new Random(42)

  val inputList = List.fill(10000)(rng.nextInt)

  @Benchmark
  def findInline(): Unit = {
    var iter = 1000
    while(iter > 0) {
      iter -= 1
      inputList.inline.find(x => (x * iter) % 13 == 0)
    }
  }
  @Benchmark
  def find(): Unit = {
    var iter = 1000
    while(iter > 0) {
      iter -= 1
      inputList.find(x => (x * iter) % 13 == 0)
    }
  }
  @Benchmark
  def reduceOptionInline(): Unit = {
    var iter = 1000
    while(iter > 0) {
      iter -= 1
      inputList.inline.reduceOption(_ + _)
    }
  }
  @Benchmark
  def reduceOption(): Unit = {
    var iter = 1000
    while(iter > 0) {
      iter -= 1
      inputList.reduceOption(_ + _)
    }
  }
}
