
object test {
   trait Foo {
      val x: Int
   }
   trait Bar {
      val y: Int
   }
   def foo: Foo = new { val x: Int = 12345; val y: Int = 54321 } with Foo with Bar {
     val normalInit: Int = 333
   } 
   def foo2: Foo = new { val x: Int = 12345; val y: Int = 54321 } with Foo with Bar
   def bar: Bar = {
     println
     new { val y = 7777 } with Bar { val yy = 8888 }
   }
   
   final class DupIterator[T](xs: T) extends {
      val x = 22222
      val init2 = 333333
   } with Foo {
     final def getElem(x: AnyRef): T = ???
   }

   final class DupIterator2[T](xs: T) extends {
      val x = 22222
      val init2 = 333333
   } with Foo
   
   lazy val a: DupIterator[Int] = ???
}
