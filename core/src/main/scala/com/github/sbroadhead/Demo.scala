package com.github.sbroadhead

/**
 * An executable demo invocable from the command line.
 */
trait Demo {
  def run(args: Seq[String]): Unit
}
