package com.github.scytrowski.sturtle.tpl.interpreter

trait Scope[F[_]] {
  type Self <: Scope[F]

  def scopeType: ScopeType

  final def getVariable(signature: VariableSignature): Option[RuntimeVariable] =
    getObject(signature).collect { case v: RuntimeVariable => v }

  final def getFunction(signature: FunctionSignature): Option[RuntimeFunction[F]] =
    getObject(signature).collect { case f: RuntimeFunction[F] => f }

  final def merge(other: Scope[F]): Scope[F] = MergedScope(this, other)

  final def onTop: Scope[F] = LayeredScope(this.scopeType, Map.empty, Some(this))

  def putObject(obj: RuntimeObject[F]): Self
  def getObject(signature: Signature): Option[RuntimeObject[F]]

  final def withinFunction(id: String): Self = withType(ScopeType.WithinFunction(id))
  final def withinLoop(id: String): Self = withType(ScopeType.WithinLoop(id))
  def withType(newType: ScopeType): Self

  def parent: Scope[F]
}

object Scope {
  def root[F[_]](id: String): Scope[F] = LayeredScope(ScopeType.Regular(id), Map.empty, None)
}

final case class LayeredScope[F[_]] private(scopeType: ScopeType,
                                            objects: Map[Signature, RuntimeObject[F]],
                                            fallbackScope: Option[Scope[F]]) extends Scope[F] {
  override type Self = LayeredScope[F]

  override def putObject(obj: RuntimeObject[F]): LayeredScope[F] =
    copy(objects = objects.updated(obj.signature, obj))

  override def getObject(signature: Signature): Option[RuntimeObject[F]] =
    objects.get(signature).orElse(fallbackScope.flatMap(_.getObject(signature)))

  override def withType(newType: ScopeType): LayeredScope[F] = copy(scopeType = newType)

  override val parent: Scope[F] = fallbackScope.getOrElse(this)
}

final case class MergedScope[F[_]](inner: Scope[F], outer: Scope[F]) extends Scope[F] {
  override type Self = MergedScope[F]

  override val scopeType: ScopeType = outer.scopeType

  override def putObject(obj: RuntimeObject[F]): MergedScope[F] = copy(outer = outer.putObject(obj))

  override def getObject(signature: Signature): Option[RuntimeObject[F]] =
    outer
      .getObject(signature)
      .orElse(inner.getObject(signature))

  override def withType(newType: ScopeType): MergedScope[F] = copy(outer = outer.withType(newType))

  override val parent: Scope[F] = inner
}
