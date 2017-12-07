package categorytheory.demo

import categorytheory.datatypes.Writer
import categorytheory.demo.support.Functions
import categorytheory.core.ops._
import categorytheory.core.implicits._

object WriterDemo extends Functions with App {

  val result: Writer[List[String], Int] = for {
    x <- moduloNumber(5)
    _ <- Writer.tell(List("Blah blah blah"))
    y <- moduloNumber(-7)
    z <- Writer.value[List[String],Int](2)
  } yield x * y * z

  println(result.run)


}
