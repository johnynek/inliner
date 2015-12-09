package com.github.johnynek.inliner.benchmark.jmh

import com.github.johnynek.inliner.InlineCollection._
import com.github.johnynek.inliner.InlineArray._

import java.util.Random
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scala.util.{Failure, Success, Try}
import org.openjdk.jmh.infra.Blackhole


object CollectionBenchmarks{
  val rng: Random = new Random(42)

  private val inputArray: Array[Int] = (List.fill(100)(4) ++ List(5)).toArray
  private val inputList = List.fill(100)(4) ++ List(5)
  private val referenceList: List[java.lang.Integer] = List.fill(100)(Integer.valueOf(4)) ++ List(Integer.valueOf(5))
  private val referenceArray: Array[java.lang.Integer] = (List.fill(100)(Integer.valueOf(4)) ++ List(Integer.valueOf(5))).toArray
  private val valueTarget: Int = 5

}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.SingleShotTime))
@Warmup(batchSize = 10000)
@Measurement(batchSize = 40000)
@Fork(value = 1)
class CollectionBenchmarks {
  import CollectionBenchmarks._

  @Benchmark
  @Measurement(batchSize = 200000)
  def findListInline(bh: Blackhole): Unit = {
    var iter = 20

    while(iter > 0) {
      iter -= 1
      bh.consume(inputList.inline.find{x => iter % 5 == x || x == 5})
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findList(bh: Blackhole): Unit = {
    var iter = 20

    while(iter > 0) {
      iter -= 1
      bh.consume(inputList.find{x => iter % 5 == x || x == 5})
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findArrayInline(bh: Blackhole): Unit = {
    var iter = 20

    while(iter > 0) {
      iter -= 1
      bh.consume(inputArray.inline.find{x => iter % 5 == x || x == 5})
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findArray(bh: Blackhole): Unit = {
    var iter = 20

    while(iter > 0) {
      iter -= 1
      bh.consume(inputArray.find{x => iter % 5 == x || x == 5})
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findReferenceListInline(bh: Blackhole): Unit = {
    var iter = 20
    val referenceTarget = Integer.valueOf(5)
    while(iter > 0) {
      iter -= 1
      bh.consume(referenceList.inline.find{x => iter % 5 == x || x.eq(referenceTarget)})
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findReferenceList(bh: Blackhole): Unit = {
    var iter = 20
    val referenceTarget = Integer.valueOf(5)
    while(iter > 0) {
      iter -= 1
      bh.consume(referenceList.find{x => iter % 5 == x || x.eq(referenceTarget)})
    }
  }
  /**
   * This is here to check if iterating using .tail is faster.
   * So far, it does not appear so.
   */
  @Benchmark
  @Measurement(batchSize = 200000)
  def findReferenceListHeadTail(bh: Blackhole): Unit = {
    var iter = 20
    val referenceTarget = Integer.valueOf(5)
    while(iter > 0) {
      iter -= 1
      var pointer = referenceList
      var running = true
      var result: Option[Int] = None
      while(running && pointer.nonEmpty) {
        val x = pointer.head
        pointer = pointer.tail
        if (iter % 5 == x || x.eq(referenceTarget)) {
          running = false
          result = Some(x)
        }
      }
      bh.consume(result)
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findReferenceArrayInline(bh: Blackhole): Unit = {
    var iter = 20
    val referenceTarget = Integer.valueOf(5)
    while(iter > 0) {
      iter -= 1
      bh.consume(referenceArray.inline.find{x => iter % 5 == x || x.eq(referenceTarget)})
    }
  }

  @Benchmark
  @Measurement(batchSize = 200000)
  def findReferenceArray(bh: Blackhole): Unit = {
    var iter = 20
    val referenceTarget = Integer.valueOf(5)
    while(iter > 0) {
      iter -= 1
      bh.consume(referenceArray.find{x => iter % 5 == x || x.eq(referenceTarget)})
    }
  }


  @Benchmark
  def reduceOptionInline(bh: Blackhole): Unit = {
    bh.consume(inputList.inline.reduceOption(_ + _))
  }
  @Benchmark
  def reduceOption(bh: Blackhole): Unit = {
    bh.consume(inputList.reduceOption(_ + _))
  }
}
