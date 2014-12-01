
object existentials {
  trait Foo
  trait Bar extends Foo
  
  trait Wrapper[T] {
    var x: T
  }
  
  val w: Wrapper[TypArg] forSome { type TypArg >: Bar <: Foo } = ???
  
  val doNothingHere: Wrapper[_ >: Bar /* troll comment */ <: Foo] = ???

  trait Pair[A, B] {
    var fst: A
    var snd: B
  }
  
  val p1: Pair[A, B] forSome {
    type A <: Bar
    type B >: Foo
  } = ???
  
  val p2: Pair[_ <: Bar, /* comment */ _ >: Foo] = ???

}
