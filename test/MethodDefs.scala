
/* Some tests ;-) */

trait Foo {

  def method1(x: Int): Unit = {
    println(x)
  }
  
  def method2a(x: Int){
    println(x + "yeah")
  }
  def method2b(x: Int) {
    println(x + "yeah")
  }
  def method2c(x: Int)  {
    println(x + "yeah")
  }
  def method2d(x: Int)        {
    println(x + "yeah")
  }

  def method3(y: Int) = println(y)

  def method4 = println("hello")

  def method5() = println("world")

  def m7a{
    println("7a")
  }
  def m7b {
    println("7b")
  }
  def m7c  {
    println("7c")
  }
  def m7d       {
    println("7c")
  }

  def m7a_2{
    val s = "7a"
    println(s)
  }
  def m7b_2 {
    val s = "7b"
    println(s)
  }
  def m7c_2  {
    val s = "7c"
    println(s)
  }
  def m7d_2       {
    val s = "7d"
    println(s)
  }

  /*
  def m8 /*the tr{o}ll comment*/ {
    println(7)
  }
  */

  def theAbstractMethod1(b: String)
  def theAbstractMethod2()
  def theAbstractMethod3

  def m9(t: Int)(u: Int*) {
	println("hi")
    println("ok")
  }

  def m10(t: Int)(u: Int*) /*some comment without equals */ {
	println("hi")
    println("ok")
  }
  
  def m9_1(t: Int)(u: Int*) {
    println("ok")
  }

  def m10_1(t: Int)(u: Int*) /*some comment without equals */ {
    println("ok")
  }

  { println("some arbitrary block of code"); println("... with some more code") }
  
  trait Again {

    def m7a
    {
      println("I'm the body of m7a!")
    }

    println("some initialization code...")
    
    def m9(t: Int)(u: Int*) 
    {
      println("hi")
      println("ok")
    }

    def m10(t: Int)(u: Int*) /*some comment without equals */ 
    {
      println("hi")
      println("ok")
    }
 
  }
}

