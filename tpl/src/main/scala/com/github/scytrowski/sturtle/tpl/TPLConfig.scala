package com.github.scytrowski.sturtle.tpl

import com.github.scytrowski.sturtle.core.TurtleRefProvider
import com.github.scytrowski.sturtle.tpl.module.Module

final case class TPLConfig[F[_]](refProvider: TurtleRefProvider[F],
                                 customModules: List[Module[F]])
