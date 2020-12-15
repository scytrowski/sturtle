package com.github.scytrowski.sturtle.tpl.types

import com.github.scytrowski.sturtle.tpl.fixture.{CommonSpecLike, RandomnessFixture}
import org.scalatest.{Inside, OptionValues}
import org.scalatest.prop.TableDrivenPropertyChecks

class ComplexTest extends CommonSpecLike with TableDrivenPropertyChecks with RandomnessFixture with OptionValues with Inside {
  "Complex" when {
    "isReal" should {
      "return true" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.real(v).isReal mustBe true
        }
      }

      "return false" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.imaginary(v).isReal mustBe false
        }
      }
    }

    "isImaginary" should {
      "return true" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.imaginary(v).isImaginary mustBe true
        }
      }

      "return false" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.real(v).isImaginary mustBe false
        }
      }
    }

    "asReal" should {
      "return some" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.real(v).asReal.value mustBe v
        }
      }

      "return nothing" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.imaginary(v).asReal.isEmpty mustBe true
        }
      }
    }

    "asImaginary" should {
      "return some" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.imaginary(v).asImaginary.value mustBe v
        }
      }

      "return nothing" in {
        forAll(Table("v", randomElements[Double](100):_*)) { v =>
          Complex.real(v).asImaginary.isEmpty mustBe true
        }
      }
    }

    "equal" when {
      "both are real" in {
        forAll(Table(("z", "w"), randomPairs[Double](1000):_*)) { case (z, w) =>
          Complex.real(z).equal(Complex.real(w)) mustBe z == w
        }
      }

      "both are imaginary" in {
        forAll(Table(("z", "w"), randomPairs[Double](1000):_*)) { case (z, w) =>
          Complex.imaginary(z).equal(Complex.imaginary(w)) mustBe z == w
        }
      }

      "other is double" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.real(v).equal(v) mustBe true
        }
      }
    }

    "nonEqual" when {
      "both are real" in {
        forAll(Table(("z", "w"), randomPairs[Double](1000):_*)) { case (z, w) =>
          Complex.real(z).nonEqual(Complex.real(w)) mustBe z != w
        }
      }

      "both are imaginary" in {
        forAll(Table(("z", "w"), randomPairs[Double](1000):_*)) { case (z, w) =>
          Complex.imaginary(z).nonEqual(Complex.imaginary(w)) mustBe z != w
        }
      }

      "other is double" in {
        forAll(Table(("z", "w"), randomPairs[Double](1000):_*)) { case (z, w) =>
          Complex.real(z).nonEqual(w) mustBe z != w
        }
      }
    }

    "lessOrEqual" when {
      "both are imaginary" in {
        forAll(Table(("z", "w"), randomPairs[Complex](1000, !_.isReal):_*)) { case (z, w) =>
          z.lessOrEqual(w) mustBe false
        }
      }

      "other is complex" in {
        Complex.real(1).lessOrEqual(Complex(2, 2)) mustBe false
      }

      "other is imaginary" in {
        Complex.real(1).lessOrEqual(Complex.imaginary(2)) mustBe false
      }

      "other is real" in {
        Complex.real(1).lessOrEqual(Complex.real(2)) mustBe true
      }

      "other is double" in {
        Complex.real(1).lessOrEqual(2) mustBe true
      }

      "both are real" in {
        forAll(Table(("p", "q"), randomPairs[Double](1000):_*)) { case (p, q) =>
          Complex.real(p).lessOrEqual(Complex.real(q)) mustBe p <= q
        }
      }
    }

    "less" when {
      "self is complex" in {
        Complex(1, 1).less(Complex.real(2)) mustBe false
      }

      "self is imaginary" in {
        Complex.imaginary(1).less(Complex.real(2)) mustBe false
      }

      "other is complex" in {
        Complex.real(1).less(Complex(2, 2)) mustBe false
      }

      "other is imaginary" in {
        Complex.real(1).less(Complex.imaginary(2)) mustBe false
      }

      "other is real" in {
        Complex.real(1).less(Complex.real(2)) mustBe true
      }

      "other is double" in {
        Complex.real(1).less(2) mustBe true
      }

      "both are real" in {
        forAll(Table(("p", "q"), randomPairs[Double](1000):_*)) { case (p, q) =>
          Complex.real(p).less(Complex.real(q)) mustBe p < q
        }
      }
    }

    "greaterOrEqual" when {
      "self is complex" in {
        Complex(2, 2).greaterOrEqual(Complex.real(1)) mustBe false
      }

      "self is imaginary" in {
        Complex.imaginary(2).greaterOrEqual(Complex.real(1)) mustBe false
      }

      "other is complex" in {
        Complex.real(2).greaterOrEqual(Complex(1, 1)) mustBe false
      }

      "other is imaginary" in {
        Complex.real(2).greaterOrEqual(Complex.imaginary(1)) mustBe false
      }

      "other is real" in {
        Complex.real(2).greaterOrEqual(Complex.real(1)) mustBe true
      }

      "other is double" in {
        Complex.real(2).greaterOrEqual(1) mustBe true
      }

      "both are real" in {
        forAll(Table(("p", "q"), randomPairs[Double](1000):_*)) { case (p, q) =>
          Complex.real(p).greaterOrEqual(Complex.real(q)) mustBe p >= q
        }
      }
    }

    "greater" when {
      "self is complex" in {
        Complex(2, 2).greater(Complex.real(1)) mustBe false
      }

      "self is imaginary" in {
        Complex.imaginary(2).greater(Complex.real(1)) mustBe false
      }

      "other is complex" in {
        Complex.real(2).greater(Complex(1, 1)) mustBe false
      }

      "other is imaginary" in {
        Complex.real(2).greater(Complex.imaginary(1)) mustBe false
      }

      "other is real" in {
        Complex.real(2).greater(Complex.real(1)) mustBe true
      }

      "other is double" in {
        Complex.real(2).greater(1) mustBe true
      }

      "both are real" in {
        forAll(Table(("p", "q"), randomPairs[Double](1000):_*)) { case (p, q) =>
          Complex.real(p).greater(Complex.real(q)) mustBe p > q
        }
      }
    }

    "positive" must {
      "return the same value" in {
        forAll(Table("c", randomElements[Complex](1000):_*)) { c =>
          c.positive mustBe c
        }
      }
    }

    "negative" must {
      "return opposite value" in {
        forAll(Table("c", randomElements[Complex](1000):_*)) { c =>
          c.negative mustBe Complex(-c.real, -c.imaginary)
        }
      }
    }

    "plus" when {
      "other is complex" in {
        forAll(Table(("c", "z"), randomPairs[Complex](1000):_*)) { case (c, z) =>
          c.plus(z) mustBe Complex(c.real + z.real, c.imaginary + z.imaginary)
        }
      }

      "other is double" in {
        forAll(Table(("c", "z"), randomElements[(Complex, Double)](1000):_*)) { case (c, z) =>
          c.plus(z) mustBe Complex(c.real + z, c.imaginary)
        }
      }
    }

    "minus" when {
      "other is complex" in {
        forAll(Table(("c", "z"), randomPairs[Complex](1000):_*)) { case (c, z) =>
          c.minus(z) mustBe Complex(c.real - z.real, c.imaginary - z.imaginary)
        }
      }

      "other is double" in {
        forAll(Table(("c", "z"), randomElements[(Complex, Double)](1000):_*)) { case (c, z) =>
          c.minus(z) mustBe Complex(c.real - z, c.imaginary)
        }
      }
    }

    "times" when {
      "other is complex" in {
        forAll(Table(("c", "z"), randomPairs[Complex](1000):_*)) { case (c, z) =>
          c.times(z) mustBe Complex(
            c.real * z.real - c.imaginary * z.imaginary,
            c.real * z.imaginary + c.imaginary * z.real
          )
        }
      }

      "other is double" in {
        forAll(Table(("c", "z"), randomElements[(Complex, Double)](1000):_*)) { case (c, z) =>
          c.times(z) mustBe Complex(c.real * z, c.imaginary * z)
        }
      }
    }

    "by" when {
      "other is complex" in {
        forAll(Table(("c", "z"), randomPairs[Complex](1000, _ != Complex.zero):_*)) { case (c, z) =>
          val result = c.by(z).value

          val multi = c * z.conjugate
          val d = z.abs * z.abs

          result.real must be (multi.real / d +- 0.001)
          result.imaginary must be (multi.imaginary / d +- 0.001)
        }
      }

      "other is double" in {
        forAll(Table(("c", "z"), randomElements[(Complex, Double)](1000, _._2 != 0):_*)) { case (c, z) =>
          val result = c.by(z).value

          result.real must be (c.real / z +- 0.001)
          result.imaginary must be (c.imaginary / z +- 0.001)
        }
      }

      "other is zero" in {
        Complex(1, 2).by(0).isEmpty mustBe true
      }
    }

    "conjugate" when {
      "self is complex" in {
        forAll(Table("c", randomElements[Complex](1000):_*)) { c =>
          c.conjugate mustBe Complex(c.real, -c.imaginary)
        }
      }

      "self is imaginary" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.imaginary(v).conjugate mustBe Complex.imaginary(-v)
        }
      }

      "self is real" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.real(v).conjugate mustBe Complex.real(v)
        }
      }
    }

    "abs" when {
      "self is complex" in {
        forAll(Table("c", randomElements[Complex](1000):_*)) { c =>
          c.abs mustBe Math.sqrt(c.real * c.real + c.imaginary * c.imaginary)
        }
      }

      "self is imaginary" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.imaginary(v).abs mustBe Math.sqrt(v * v)
        }
      }

      "self is real" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.real(v).abs mustBe Math.sqrt(v * v)
        }
      }
    }
  }
}
