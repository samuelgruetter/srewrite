
trait T[_] {
  def foo[_] = 0
  def foo2[_, B] = 0
  def bar[M[_]] = 0
}

trait T2[_, K] {}

