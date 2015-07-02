package se.fnord.conway

import java.util.concurrent.atomic.AtomicBoolean

class Once {
  val done = new AtomicBoolean(false)
  def apply(f : => Unit) {
    if (done.compareAndSet(false, true))
      f
  }
}

object Once {
  def apply() = new Once()
}
