
object test {
   trait Foo {
      val x: Int
   }
   trait Bar {
      val y: Int
   }
   def foo: Foo = new { val x: Int = 12345; val z: Int = 54321 } with Foo with Bar {
     val normalInit: Int = 333
   }
   def bar: Bar = {
     println
     new { val y = 7777 } with Bar { val yy = 8888 }
   }
}
