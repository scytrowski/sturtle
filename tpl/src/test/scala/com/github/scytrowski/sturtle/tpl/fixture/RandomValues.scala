package com.github.scytrowski.sturtle.tpl.fixture

import com.github.scytrowski.sturtle.core.geometry.{Angle, Point, Vector}
import com.github.scytrowski.sturtle.core.graphics.Color
import com.github.scytrowski.sturtle.tpl.parser.Token.NameToken
import com.github.scytrowski.sturtle.tpl.types.Complex
import org.scalatest.matchers.must.Matchers
import shapeless.Lazy

import scala.util.Random

trait RandomValues { this: Matchers =>
  protected def randomTriples[A: RandomGenerator](count: Int, predicate: A => Boolean = (_: A) => true): List[(A, A, A)] =
    randomPairs[A](count, predicate)
      .zip(randomElements[A](count, predicate))
      .map { case ((f, s), t) => (f, s, t) }

  protected def randomPairs[A: RandomGenerator](count: Int, predicate: A => Boolean = (_: A) => true): List[(A, A)] =
    randomElements[A](count, predicate)
      .zip(randomElements[A](count, predicate))

  protected def randomElements[A](count: Int, predicate: A => Boolean = (_: A) => true)(implicit gen: RandomGenerator[A]): List[A] =
    LazyList
      .continually(gen.generate)
      .filter(predicate)
      .take(count)
      .toList

  protected implicit val randomInt: RandomGenerator[Int] = RandomGenerator(Random.nextInt(Int.MaxValue))
  protected implicit def randomDouble(implicit params: RandomNumberParameters): RandomGenerator[Double] = RandomGenerator(Random.between(params.minValue, params.maxValue))
  protected implicit def randomComplex(implicit params: RandomNumberParameters): RandomGenerator[Complex] =
    for {
      r <- randomDouble
      i <- randomDouble
    } yield Complex(r, i)
  protected implicit val randomString: RandomGenerator[String] = RandomGenerator {
    val l = Random.nextInt(2048)
    val s = Random.nextString(l)
    s
      .replace("\"", "")
      .replace("\n", "")
  }
  protected implicit val randomName: RandomGenerator[NameToken] = RandomGenerator {
    val l = Random.nextInt(2048) + 1
    val head = randomLetter
    val tail = List.fill(l - 1)(randomLetterOrDigit)
    NameToken((head +: tail).mkString)
  }
  protected implicit def randomPoint(implicit params: RandomNumberParameters): RandomGenerator[Point] =
    for {
      x <- randomDouble
      y <- randomDouble
    } yield Point(x, y)
  protected implicit def randomVector(implicit params: RandomNumberParameters): RandomGenerator[Vector] =
    for {
      dx <- randomDouble
      dy <- randomDouble
    } yield Vector(dx, dy)
  protected implicit def randomAngle(implicit params: RandomNumberParameters): RandomGenerator[Angle] =
    for {
      v <- randomDouble
    } yield Angle(v)
  protected implicit def randomColor(implicit params: RandomNumberParameters): RandomGenerator[Color] =
    for {
      r <- randomDouble
      g <- randomDouble
      b <- randomDouble
    } yield Color.decimal(r, g, b)
  protected implicit def randomPair[A, B](implicit aGen: Lazy[RandomGenerator[A]], bGen: Lazy[RandomGenerator[B]]): RandomGenerator[(A, B)] =
    for {
      a <- aGen.value
      b <- bGen.value
    } yield a -> b

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

  protected trait RandomGenerator[+A] { self =>
    def generate: A

    def map[B](f: A => B): RandomGenerator[B] = new RandomGenerator[B] {
      override def generate: B = f(self.generate)
    }

    def flatMap[B](f: A => RandomGenerator[B]): RandomGenerator[B] = new RandomGenerator[B] {
      override def generate: B = f(self.generate).generate
    }
  }

  object RandomGenerator {
    def apply[A](random: => A): RandomGenerator[A] = new RandomGenerator[A] {
      override def generate: A = random
    }
  }

  protected final case class RandomNumberParameters(minValue: Int, maxValue: Int)
}
