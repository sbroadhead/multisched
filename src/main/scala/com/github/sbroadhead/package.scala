package com.github

package object sbroadhead {
  /**
   * Returns the implicit [[Generator]] argument.
   * @param gen The generator.
   * @tparam T The type of generator to return.
   * @return The generator.
   */
  def generator[T](implicit gen: Generator[T]): Generator[T] = gen
}
