package dh.lang

import cats.Id
import dh.lang.data.{RefId, RefName, RefNs}
import dh.lang.linker.{CalcTyped, Result}
import dh.lang.core.*

object Demo:
  val notOpened = "not_opened"
  val clientsCount = "clients_count"
  val notOpenedRel = "not_opened_rel"

  val exprNotOpenedRel = s"$notOpened / $clientsCount"
  val exprNotOpenedReq =
    s"""iif($notOpenedRel >= 0.2, red, $notOpenedRel >= 0.1, yellow, green)""".stripMargin

  val accountOpenedAtMin = "account_opened_at_min"
  val exprAccountOpenedAtMin = s"""$accountOpenedAtMin >= toDate("2023-08- 05")"""

  val refer1 = new Refer[Id]:
    def ref(set: Option[RefNs], name: RefName): Option[CalcTyped] =
      name match
        case `notOpened` => Some(CalcTyped(s"ns.$notOpened".mkRefId, TDecimal))
        case `clientsCount` => Some(CalcTyped(s"ns.$clientsCount".mkRefId, TDecimal))
        case _ => None

  def demo1(): BigDecimal =
    val term = api.parse(exprNotOpenedRel).fold(s => throw new Exception(s"Unable to parse expr: $s"), identity)
    println(s"Parsed term: $term")

    given Refer[Id] = refer1
    val Result(termL, depends) = api.link[Id](term)

    println(s"Linked term: $termL")
    println(s"Term depends on: $depends")

    val byteCode = api.compile(termL, TDecimal).fold(errs => throw new Exception(s"Compile errors: $errs"), identity)
    println("Compiled!")
    val res = api.run(
      byteCode, TDecimal,
      Map(s"ns.$notOpened".mkRefId -> BigDecimal(100), s"ns.$clientsCount".mkRefId -> BigDecimal(499))
    )
    println(s"Result: $res")
    res.fold(err => throw new Exception(s"Runtime error: $err"), identity)

  val refer2 = new Refer[Id]:
    def ref(set: Option[RefNs], name: RefName): Option[CalcTyped] =
      name match
        case `notOpened` => Some(CalcTyped(s"ns.$notOpened".mkRefId, TDecimal))
        case `clientsCount` => Some(CalcTyped(s"ns.$clientsCount".mkRefId, TDecimal))
        case `notOpenedRel` => Some(CalcTyped(s"ns.$notOpenedRel".mkRefId, TDecimal))
        case _ => None

  def demo2(relRes: BigDecimal): RedYellowGreen =
    val term = api.parse(exprNotOpenedReq).fold(s => throw new Exception(s"Unable to parse expr: $s"), identity)
    println(s"Parsed term: $term")

    given Refer[Id] = refer2
    val Result(termL, depends) = api.link[Id](term)

    println(s"Linked term: $termL")
    println(s"Term depends on: $depends")

    val byteCode = api.compile(termL, TRYG).fold(errs => throw new Exception(s"Compile errors: $errs"), identity)
    println("Compiled!")
    val res = api.run(
      byteCode, TRYG,
      Map(s"ns.$notOpenedRel".mkRefId -> relRes)
    )
    println(s"Result: $res")
    res.fold(err => throw new Exception(s"Runtime error: $err"), identity)

  val refer3 = new Refer[Id]:
    def ref(set: Option[RefNs], name: RefName): Option[CalcTyped] =
      name match
        case `accountOpenedAtMin` => Some(CalcTyped(s"ns.$accountOpenedAtMin".mkRefId, TDate))
        case _ => None

  def demo3(): Unit =
    val term = api.parse(exprAccountOpenedAtMin).fold(s => throw new Exception(s"Unable to parse expr: $s"), identity)
    println(s"Parsed term: $term")

    given Refer[Id] = refer3
    val Result(termL, depends) = api.link[Id](term)

    println(s"Linked term: $termL")
    println(s"Term depends on: $depends")

    val compileErrors = api.compile(termL, TBool).fold(identity, _ => throw new Exception(s"No compile errors"))
    println(s"Compile errors: $compileErrors")

  @main def run: Unit =
    val fst = demo1()
    demo2(fst)
    demo3()
    ()

  extension (s: String) def mkRefId: RefId = RefId(s).getOrElse(throw new Exception(s"Illegal refId $s"))