/*
 * Copyright 2020-2022 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.effect

private final class ArrayStack[A <: AnyRef | Null](
    private[this] var buffer: Array[AnyRef | Null],
    private[this] var index: Int) {

  def this(initBound: Int) =
    this(new Array[AnyRef | Null](initBound), 0)

  def this() = this(null.asInstanceOf[Array[AnyRef | Null]], 0)

  def init(bound: Int): Unit = {
    buffer = new Array(bound)
    index = 0
  }

  def push(a: A): Unit = {
    checkAndGrow()
    buffer(index) = a
    index += 1
  }

  // TODO remove bounds check
  def pop(): A = {
    index -= 1
    val back = buffer(index).asInstanceOf[A]
    buffer(index) = null.asInstanceOf[A] // avoid memory leaks
    back
  }

  def peek(): A = buffer(index - 1).asInstanceOf[A]

  def isEmpty(): Boolean = index <= 0

  // to allow for external iteration
  def unsafeBuffer(): Array[AnyRef | Null] = buffer
  def unsafeIndex(): Int = index

  def unsafeSet(newI: Int): Unit = {
    var i = newI
    while (i < index) {
      buffer(i) = null.asInstanceOf[A]
      i += 1
    }

    index = newI
  }

  def invalidate(): Unit = {
    index = 0
    buffer = null.asInstanceOf[Array[AnyRef | Null]]
  }

  private[this] def checkAndGrow(): Unit =
    if (index >= buffer.length) {
      val len = buffer.length
      val buffer2 = new Array[AnyRef | Null](len * 2)
      System.arraycopy(buffer, 0, buffer2, 0, len)
      buffer = buffer2
    }
}
