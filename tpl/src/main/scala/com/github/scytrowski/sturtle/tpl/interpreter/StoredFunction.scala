package com.github.scytrowski.sturtle.tpl.interpreter

final case class StoredFunction(signature: FunctionSignature, code: TPLCode.WithPush)
