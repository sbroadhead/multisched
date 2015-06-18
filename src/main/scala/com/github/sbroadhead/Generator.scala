package com.github.sbroadhead

import scala.util.Random

/**
 * A generator of unique values of `T`. No value should be generated twice.
 * @tparam T The type to generate.
 */
trait Generator[T] {
  def generate: T
}

/**
 * Companion object to [[Generator]].
 */
object Generator {
  /**
   * Generator for random integers.
   */
  implicit val intGenerator = new Generator[Int] {
    override def generate: Int = Random.nextInt()
  }
}
