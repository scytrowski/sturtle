package com.github.scytrowski.sturtle.tpl.interpreter

sealed abstract class ValueType {
  type Repr <: Value

  def extract(value: Value): Option[Repr]
}

object ValueType {
  type Aux[V <: Value] = ValueType { type Repr = V }

  case object AnyType extends AnyType {
    override type Repr = Value

    override def extract(value: Value): Option[Value] = Some(value)
  }
  case object BooleanType extends ValueType {
    override type Repr = BooleanValue

    override def extract(value: Value): Option[BooleanValue] = value match {
      case bool: BooleanValue => Some(bool)
      case _ => None
    }
  }
  case object NumberType extends ValueType {
    override type Repr = NumberValue

    override def extract(value: Value): Option[NumberValue] = value match {
      case number: NumberValue => Some(number)
      case _ => None
    }
  }
  case object StringType extends ValueType {
    override type Repr = StringValue

    override def extract(value: Value): Option[StringValue] = value match {
      case string: StringValue => Some(string)
      case _ => None
    }
  }

  sealed abstract class AnyType extends ValueType
}


