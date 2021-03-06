package oscar.utils

object Time {

  def times(s: String)(block:  => Unit): Unit ={
    val t0 = System.currentTimeMillis()
    block
    println("Executed " + s + " in " + (System.currentTimeMillis() - t0))
  }
}