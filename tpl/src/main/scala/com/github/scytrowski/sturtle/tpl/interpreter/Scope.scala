package com.github.scytrowski.sturtle.tpl.interpreter

trait Scope {
  type Self <: Scope

  def scopeType: ScopeType

  final def getVariable(signature: VariableSignature): Option[RuntimeVariable] =
    getObject(signature).collect { case v: RuntimeVariable => v }

  final def getFunction(signature: FunctionSignature): Option[RuntimeFunction] =
    getObject(signature).collect { case f: RuntimeFunction => f }

  def putObject(obj: RuntimeObject): Self
  def getObject(signature: Signature): Option[RuntimeObject]

  def withinFunction: Self
  def withinLoop: Self

  def parent: Scope
  def onTop: Scope
}

object Scope {
  def root: Scope = LayeredScope(ScopeType.Regular, Map.empty, None)
}

trait MapBasedScope extends Scope {
  def objects: Map[Signature, RuntimeObject]

  protected def fallbackScope: Option[Scope]

  final override def getObject(signature: Signature): Option[RuntimeObject] = objects.get(signature)
}

final case class LayeredScope private(scopeType: ScopeType,
                                      objects: Map[Signature, RuntimeObject],
                                      protected val fallbackScope: Option[Scope]) extends MapBasedScope {
  override type Self = LayeredScope

  override def putObject(obj: RuntimeObject): LayeredScope =
    copy(objects = objects.updated(obj.signature, obj))

  override def withinFunction: LayeredScope = copy(scopeType = ScopeType.WithinFunction)

  override def withinLoop: LayeredScope = copy(scopeType = ScopeType.WithinLoop)

  override val parent: Scope = fallbackScope.getOrElse(this)

  override def onTop: Scope = LayeredScope(scopeType, Map.empty, Some(this))
}

final case class RootScope private(scopeType: ScopeType,
                                   variables: Map[VariableSignature, StoredVariable],
                                   functions: Map[FunctionSignature, StoredFunction])

sealed abstract class ScopeType

object ScopeType {
  case object Regular extends ScopeType
  case object WithinFunction extends ScopeType
  case object WithinLoop extends ScopeType
}
