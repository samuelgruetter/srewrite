package scala.print.plugin

import scala.tools.nsc.Global

trait WithGlobal {
  val global: Global
}
