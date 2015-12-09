package com.github.johnynek.inliner.benchmark.jmh

import com.github.johnynek.inliner.InlineRange._

import java.util.Random
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.util.{Failure, Success, Try}
import org.openjdk.jmh.infra.Blackhole

object RangeBenchmarks {
  val inputRange = (-10 to 10000)
}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.SingleShotTime))
@Warmup(batchSize = 10000)
@Measurement(batchSize = 40000)
@Fork(value = 1)
class RangeBenchmarks {
  import RangeBenchmarks._

  @Benchmark
  @Measurement(batchSize = 20000)
  def foreachInline(bh: Blackhole): Unit = {
    var iter = 20
    while(iter > 0) {
      iter -= 1
      var sum = 0
      inputRange.inline.foreach(sum += _)
      bh.consume(sum)
    }
  }
  @Benchmark
  @Measurement(batchSize = 20000)
  def foreach(bh: Blackhole): Unit = {
    var iter = 20
    while(iter > 0) {
      iter -= 1
      var sum = 0
      inputRange.foreach(sum += _)
      bh.consume(sum)
    }
  }
}
