package scala.print.plugin

import java.io.OutputStream

class NullOutputStream extends OutputStream {
  override def write(i: Int): Unit = { /* do nothing */ } 
}

