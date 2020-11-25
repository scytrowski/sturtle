package com.github.scytrowski.sturtle.tpl.fixture

import org.scalatest.matchers.must.Matchers

import scala.util.Random

trait RandomnessFixture { this: Matchers =>
  protected def randomInts(count: Int): List[Int] = List.fill(count)(Random.nextInt(Int.MaxValue))

  protected def randomDoubles(count: Int): List[Double] = List.fill(count)(Random.nextDouble())

  protected def randomStrings(count: Int): List[String] = List.fill(count)(randomString)

  protected def randomNames(count: Int): List[String] = List.fill(count)(randomName)

  protected def randomString: String = {
    val l = Random.nextInt(2048)
    val s = Random.nextString(l)
    s
      .replace("\"", "")
      .replace("\n", "")
  }

  protected def randomName: String = {
    val l = Random.nextInt(2048) + 1
    val head = randomLetter
    val tail = List.fill(l - 1)(randomLetterOrDigit)
    (head +: tail).mkString
  }

  protected def randomLetterOrDigit: Char = randomElement(digits ++ letters)

  protected def randomLetter: Char = randomElement(letters)

  protected def randomDigit: Char = randomElement(digits)

  protected def randomElement[A](seq: Seq[A]): A = {
    seq.isEmpty mustBe false

    val i = Random.nextInt(seq.length)
    seq(i)
  }

  protected val letters: Seq[Char] = 'a'.to('z') ++ 'A'.to('Z')
  protected val digits: Seq[Char] = '0'.to('9')
}
