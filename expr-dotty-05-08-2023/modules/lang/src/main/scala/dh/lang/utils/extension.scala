package dh.lang

import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Names.{SimpleName, TypeName}

extension (str: String)
  def mkTermName: SimpleName = Names.termName(str)
  def mkTypeName: TypeName   = Names.typeName(str)
