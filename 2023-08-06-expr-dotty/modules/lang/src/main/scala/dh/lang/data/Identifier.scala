package dh.lang.data

import cats.Hash
import dh.lang.data.{Like, StringNewType}

type RefId = RefId.T
object RefId extends Like(regex.calculation):
  given Hash[RefId] = Hash.fromUniversalHashCode

type RefNs = RefNs.T
object RefNs extends Like(regex.ident)

type RefName = RefName.T
object RefName extends Like(regex.refName)

type Identifier = Identifier.T
object Identifier extends StringNewType:
  given Hash[Identifier]                        = Hash.fromUniversalHashCode
  extension (s: String) def mkIdent: Identifier = Identifier(s)

object regex:
  val ident       = "[a-zA-Z][a-zA-Z0-9_]*"
  val id          = s"$ident\\.$ident"
  val refName     = s"$ident(\\.$ident)?"
  val calculation = s"$ident\\.$refName"
