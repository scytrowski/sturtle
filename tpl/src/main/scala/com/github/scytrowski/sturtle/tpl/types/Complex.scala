package com.github.scytrowski.sturtle.tpl.types

import com.github.scytrowski.sturtle.tpl.types.Complex.ComplexNumeric

final case class Complex(real: Double, imaginary: Double) {
  def isReal: Boolean = imaginary == 0
  def isImaginary: Boolean = real == 0

  def asReal: Option[Double] = if (isReal) Some(real) else None
  def asImaginary: Option[Double] = if (isImaginary) Some(imaginary) else None

  def ==[N: ComplexNumeric](other: N): Boolean = equal(other)
  def equal[N: ComplexNumeric](other: N): Boolean =
    this.equals(ComplexNumeric[N].toComplex(other))

  def !=[N: ComplexNumeric](other: N): Boolean = nonEqual(other)
  def nonEqual[N: ComplexNumeric](other: N): Boolean = !equal(other)

  def <=[N: ComplexNumeric](other: N): Boolean = lessOrEqual(other)
  def lessOrEqual[N: ComplexNumeric](other: N): Boolean =
    less(other) || equal(other)

  def <[N: ComplexNumeric](other: N): Boolean = less(other)
  def less[N: ComplexNumeric](other: N): Boolean =
    asReal
      .zip(ComplexNumeric[N].toComplex(other).asReal)
      .fold(false) { case (l, r) => l < r }

  def >=[N: ComplexNumeric](other: N): Boolean = greaterOrEqual(other)
  def greaterOrEqual[N: ComplexNumeric](other: N): Boolean =
    greater(other) || equal(other)

  def >[N: ComplexNumeric](other: N): Boolean = greater(other)
  def greater[N: ComplexNumeric](other: N): Boolean =
    asReal
      .zip(ComplexNumeric[N].toComplex(other).asReal)
      .fold(false) { case (l, r) => l > r }

  def unary_+ : Complex = positive
  def positive: Complex = Complex(+real, +imaginary)

  def unary_- : Complex = negative
  def negative: Complex = Complex(-real, -imaginary)

  def +[N: ComplexNumeric](other: N): Complex = plus(other)
  def plus[N: ComplexNumeric](other: N): Complex = {
    val otherComplex = ComplexNumeric[N].toComplex(other)
    Complex(real + otherComplex.real, imaginary + otherComplex.imaginary)
  }

  def -[N: ComplexNumeric](other: N): Complex = minus(other)
  def minus[N: ComplexNumeric](other: N): Complex = {
    val otherComplex = ComplexNumeric[N].toComplex(other)
    Complex(real - otherComplex.real, imaginary - otherComplex.imaginary)
  }

  def *[N: ComplexNumeric](other: N): Complex = times(other)
  def times[N: ComplexNumeric](other: N): Complex = {
    val otherComplex = ComplexNumeric[N].toComplex(other)
    Complex(
      real * otherComplex.real - imaginary * otherComplex.imaginary,
      real * otherComplex.imaginary + imaginary * otherComplex.real
    )
  }

  def /[N: ComplexNumeric](other: N): Option[Complex] = by(other)
  def by[N: ComplexNumeric](other: N): Option[Complex] = {
    val otherComplex = ComplexNumeric[N].toComplex(other)
    val divisor = Math.pow(otherComplex.abs, 2)
    if (divisor != 0) {
      val dividend = this * otherComplex.conjugate
      Some(Complex(dividend.real / divisor, dividend.imaginary / divisor))
    } else
      None
  }

  def **[N: ComplexNumeric](other: N): Complex = toPowerOf(other)
  def toPowerOf[N: ComplexNumeric](other: N): Complex =
    ln.fold(Complex.zero)(l => (l * other).exp)

  def exp: Complex = Complex(Math.cos(imaginary), Math.sin(imaginary)) * Math.exp(real)

  def log[N: ComplexNumeric](base: N): Option[Complex] = {
    val baseComplex = ComplexNumeric[N].toComplex(base)
    if (baseComplex.isValidLogBase)
      for {
        nominator   <- ln
        denominator <- baseComplex.ln
        res         <- nominator / denominator
      } yield res
    else
      None
  }

  def ln: Option[Complex] =
    if (!isZero)
      Some(Complex(Math.log(abs), arg))
    else
      None

  def sin: Complex =
    Complex(
      Math.sin(real) * Math.cosh(imaginary),
      Math.cos(real) * Math.sinh(imaginary)
    )

  def cos: Complex =
    Complex(
      Math.cos(real) * Math.cosh(imaginary),
      Math.sin(real) * Math.sinh(imaginary)
    )

  def isValidLogBase: Boolean = asReal.fold(true)(r => r > 0 && r != 1)

  def isZero: Boolean = this == Complex.zero

  def conjugate: Complex = Complex(real, -imaginary)

  def abs: Double = Math.sqrt(real * real + imaginary * imaginary)

  def arg: Double = Math.atan2(imaginary, real)

  override def toString: String =
    isReal -> isImaginary match {
      case (false, false) => s"$real${if (imaginary < 0) "-" else "+"}${Math.abs(imaginary)}i"
      case (false, _)     => s"${imaginary}i"
      case (_, false)     => s"$real"
      case (true, true)   => "0"
    }
}

object Complex {
  val zero: Complex = real(0)
  val one: Complex = real(1)
  val unit: Complex = imaginary(1)
  def real(value: Double): Complex = Complex(value, 0)
  def imaginary(value: Double): Complex = Complex(0, value)

  trait ComplexNumeric[N] {
    def toComplex(n: N): Complex
  }

  object ComplexNumeric {
    def apply[N](implicit numeric: ComplexNumeric[N]): ComplexNumeric[N] = numeric
  }

  implicit val forComplex: ComplexNumeric[Complex] = identity

  implicit def fromRealNumeric[N](implicit numeric: Numeric[N]): ComplexNumeric[N] =
    n => Complex(numeric.toDouble(n), 0)
}
