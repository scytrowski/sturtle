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

    "toPowerOf" when {
      "other is complex" in {
        forAll(Table(("c", "z"), randomElements[(Complex, Complex)](1000, { case (c, _) => !c.isZero }):_*)) { case (c, z) =>
          c.toPowerOf(z) mustBe (c.ln.value * z).exp
        }
      }

      "other is double" in {
        forAll(Table(("c", "z"), randomElements[(Complex, Double)](1000, { case (c, _) => !c.isZero }):_*)) { case (c, z) =>
          c.toPowerOf(z) mustBe (c.ln.value * z).exp
        }
      }

      "self is zero" in {
        forAll(Table("z", randomElements[Complex](100):_*)) { z =>
          Complex.zero.toPowerOf(z) mustBe Complex.zero
        }
      }
    }

    "exp" when {
      "self is complex" in {
        forAll(Table("z", randomElements[Complex](1000, !_.isReal):_*)) { z =>
          z.exp mustBe Complex(
            Math.exp(z.real) * Math.cos(z.imaginary),
            Math.exp(z.real) * Math.sin(z.imaginary)
          )
        }
      }

      "self is real" in {
        forAll(Table("v", randomElements[Double](100, v => Math.abs(v) < 100):_*)) { v =>
          Complex.real(v).exp mustBe Complex.real(Math.exp(v))
        }
      }
    }

    "log" when {
      "base is valid" in {
        forAll(Table(("z", "w"), randomElements[(Complex, Complex)](1000, { case (z, w) => !z.isZero && w.isValidLogBase }):_*)) { case (z, w) =>
          z.log(w).value mustBe (z.ln.value / w.ln.value).value
        }
      }

      "number is 0" in {
        forAll(Table("b", randomElements[Complex](10, _.isValidLogBase):_*)) { b =>
          Complex.zero.log(b).isEmpty mustBe true
        }
      }

      "base is negative real" in {
        forAll(Table(("z", "b"), randomElements[(Complex, Double)](10, { case (z, b) => !z.isZero && b < 0 }):_*)) { case (z, b) =>
          z.log(b).isEmpty mustBe true
        }
      }

      "base is 0" in {
        forAll(Table("z", randomElements[Complex](10, !_.isZero):_*)) { z =>
          z.log(0).isEmpty mustBe true
        }
      }

      "base is 1" in {
        forAll(Table("z", randomElements[Complex](10, !_.isZero):_*)) { z =>
          z.log(1).isEmpty mustBe true
        }
      }
    }

    "ln" when {
      "non zero" in {
        forAll(Table("z", randomElements[Complex](1000, _ != Complex.zero):_*)) { z =>
          val expectedRe = Math.log(z.abs)
          val expectedIm = Math.atan2(z.imaginary, z.real)

          z.ln.value mustBe Complex(expectedRe, expectedIm)
        }
      }

      "zero" in {
        Complex.zero.ln.isEmpty mustBe true
      }
    }

    "sin" when {
      "self is complex" in {
        forAll(Table("z", randomElements[Complex](1000, !_.isReal):_*)) { z =>
          z.sin mustBe Complex(
            Math.sin(z.real) * Math.cosh(z.imaginary),
            Math.cos(z.real) * Math.sinh(z.imaginary)
          )
        }
      }

      "self is real" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.real(v).sin mustBe Complex.real(Math.sin(v))
        }
      }
    }

    "cos" when {
      "self is complex" in {
        forAll(Table("z", randomElements[Complex](1000, !_.isReal):_*)) { z =>
          z.cos mustBe Complex(
            Math.cos(z.real) * Math.cosh(z.imaginary),
            Math.sin(z.real) * Math.sinh(z.imaginary)
          )
        }
      }

      "self is real" in {
        forAll(Table("v", randomElements[Double](1000):_*)) { v =>
          Complex.real(v).cos mustBe Complex.real(Math.cos(v))
        }
      }
    }

    "isValidLogBase" when {
      "number has imaginary part" in {
        forAll(Table("z", randomElements[Complex](1000, _.imaginary != 0):_*)) { z =>
          z.isValidLogBase mustBe true
        }
      }

      "number is real" in {
        forAll(Table("v", randomElements[Double](100, v => v > 0 && v != 1):_*)) { v =>
          Complex.real(v).isValidLogBase mustBe true
        }
      }

      "number is negative real" in {
        forAll(Table("v", randomElements[Double](10, _ < 0):_*)) { v =>
          Complex.real(v).isValidLogBase mustBe false
        }
      }

      "number is 0" in {
        Complex.zero.isValidLogBase mustBe false
      }

      "number is 1" in {
        Complex.one.isValidLogBase mustBe false
      }
    }

    "isZero" when {
      "number is 0" in {
        Complex.zero.isZero mustBe true
      }

      "otherwise" in {
        forAll(Table("z", randomElements[Complex](1000, _ != Complex.zero):_*)) { z =>
          z.isZero mustBe false
        }
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

    "arg" in {
      forAll(Table("z", randomElements[Complex](1000):_*)) { z =>
        z.arg mustBe Math.atan2(z.imaginary, z.real)
      }
    }
  }
}
