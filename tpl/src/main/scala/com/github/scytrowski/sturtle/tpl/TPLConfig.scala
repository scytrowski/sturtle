package com.github.scytrowski.sturtle.tpl

import com.github.scytrowski.sturtle.core.TurtleRefProvider

final case class TPLConfig[F[_]](refProvider: TurtleRefProvider[F])
