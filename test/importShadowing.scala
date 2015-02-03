
// This compiles in Scala 2, but not in dotty:

// import an object which is only an object and no class:
import scala.sys.process.BasicIO

// a class with the same name:
class BasicIO {
  println(BasicIO.BufferSize)
  def foo = new BasicIO
}
