# Inliner
[![Build
Status](https://travis-ci.org/johnynek/inliner.svg)](https://travis-ci.org/johnynek/inliner)

Inliner is a collection of scala macros to inline and optimize idiomatic scala into while loops or nested if/else statements. The purpose is to allow idiomatic scala without having to give up performance.

## How to use?
Add `val inliner = ProjectRef(uri("git://github.com/johnynek/inliner.git"), "core")` to your sbt project, then add `.dependsOn(inliner)`
to any project where you want to use the code. Then do:
```scala
import com.github.johnynek.inliner.{InlineTry, InlineOption, InlineCollection}
import InlineTry._
import InlineOption._
import InlineCollection._
```
Generally you import methods from an object and replace calls like `x.method` with `x.inline.method`

### Collections
There are `.inline` versions of the following TraversableOnce methods: `find`, `forall`, `foldLeft`, `foreach`, `reduceOption`.
```scala
import com.github.johnynek.inliner.InlineCollection._
object Test {
  println(List(0, 1, 2, 4, 8, 16).inline.foldLeft(0)(_ + _))
}
```
Expands the foldLeft into:
```scala
  val it$macro$1 = immutable.this.List.apply[Int](0, 1, 2, 4, 8, 16).toIterator;
  var x$1 = 0;
  while$1(){
    if (it$macro$1.hasNext)
      {
        {
          val x$2 = it$macro$1.next;
          x$1 = x$1.+(x$2)
        };
        while$1()
      }
    else
      ()
  };
  x$1
```
### scala.util.Try
Normally, creating a Try means a call-by-name parameter, which requires an allocation and a method call. With a macro, we can directly inline into a try/catch block:
```scala
import com.github.johnynek.inliner.InlineTry._
def halfEven(x: Int): Int = { require(x % 2 == 0, "not even: " + x); x/2 }
inlineTry {
  val x = halfEven(42)
  val y = halfEven(43)
  x * y
}
```
which, at the REPL, expands to:
```scala
try {
  new _root_.scala.util.Success[Int]({
    val x = $line4.$read.$iw.$iw.$iw.$iw.halfEven(42);
    val y = $line4.$read.$iw.$iw.$iw.$iw.halfEven(43);
    x.*(y)
  })
} catch {
  case _root_.scala.util.control.NonFatal((e @ _)) => new scala.util.Failure[Nothing](e)
}
```
Similarly, for expressions can be expanded into nested if/else:
```scala
 for {
   a <- inlineTry { assert(o1 > 0); o1 }.inline
   b <- o2(a).inline
   c <- o3(b).inline
 } yield c
```
expands to:
```scala
{
  val opt$macro$13 = (try {
    new scala.util.Success[Int]({
      scala.this.Predef.assert(o1.>(0));
      o1
    })
  } catch {
    case _root_.scala.util.control.NonFatal((e @ _)) => new scala.util.Failure[Nothing](e)
  }: scala.util.Try[Int]);
  if (opt$macro$13.isSuccess)
    {
      val a = opt$macro$13.get;
      try {
        ({
          val opt$macro$12 = o2.apply(a);
          if (opt$macro$12.isSuccess)
            {
              val b = opt$macro$12.get;
              try {
                (o3.apply(b): scala.util.Try[Long])
              } catch {
                case _root_.scala.util.control.NonFatal((e @ _)) => new scala.util.Failure[Nothing](e)
              }
            }
          else
            opt$macro$12.asInstanceOf[scala.util.Try[Long]]
        }: scala.util.Try[Long])
      } catch {
        case _root_.scala.util.control.NonFatal((e @ _)) => new scala.util.Failure[Nothing](e)
      }
    }
  else
    opt$macro$13.asInstanceOf[_root_.scala.util.Try[Long]]
}

```
This gives you inlined versions of: `filter`, `flatMap`, `flatten`, `fold`, `foreach`, `getOrElse`, `map`, `orElse`.

### Option
Similar to Try, you can inline for loops using `.inline` on methods `filter`, `flatMap`, `flatten`, `fold`, `foreach`, `getOrElse`, `map`, `orElse`.

# When should I use Inliner?
Macros are not as reliable as they could be. You should probably only use this library for inner loops that have been profiled. Optimizing without profiling is usually not profitable. Once you find a method that needs maximum optimization, Inliner may allow you to keep idiomatic code with minor modifications to get maximum performance.

# Future Work
Check the issues, but generally support for more classes (such as Either) or constructs (such as PartialFunction literals) would be useful. Also, optimizing some of the trees would be really interesting. Once we have a full tree we can see that some of the branches will never be taken in large for-expressions. Also, we could port this approach to a whitebox macro such as `def inline(x: Any): Any` which could do whole expression optimization along the lines we have here without manually calling .inline. This would have the benefit of being able to optimize things like:
```scala
myList
  .map { x => (x, 1) }
  .reduceOption { case (la, lb), (ra, rb) => (la + ra, lb + lb) }
```
to an expression like:
```scala
val it = myList.iterator
if (it.hasNext) {
  val head = it.next
  var result1 = head
  var result2 = 1
  while(it.hasNext) {
    val item = it.next
    val item1 = item
    val item2 = 1
    result1 = result1 + item1
    result2 = result2 + item2
  }
  Some((result1, result2))
} else None
```

# Authors
Best to check the commit history, but this was started by [Oscar Boykin](https://twitter.com/posco).
