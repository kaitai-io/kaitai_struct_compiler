package io.kaitai.struct.exprlang

import fastparse._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.format._

import scala.collection.immutable.SortedMap

/**
  * Structs object holds parser for Kaitai Struct C-like language (.ksc),
  * which works as alternative to YAML-based .ksy format. Internal representation
  * of parsed structures is the same as for .ksy files (io.kaitai.struct.format),
  * and all the downstream processing (e.g. precompilation + compilation) should
  * work almost the same.
  */
object Structs {
  // Implicit rule which consume input in `~` and `.rep` operators
  implicit val whitespace: P[_] => P[Unit] = { implicit ctx: ParsingRun[_] => Lexical.wscomment }

  def primitiveTypeRef[$: P]: P[DataType] =
    P(
      "u1" |
      "u2le" | "u4le" | "u8le" |
      "u2be" | "u4be" | "u8be" |
      "s1" |
      "s2le" | "s4le" | "s8le" |
      "s2be" | "s4be" | "s8be" |
      "f4le" | "f8le" |
      "f4be" | "f8be"
    ).!.map {
      case "u1" => Int1Type(signed = false)
      case "u2le" => IntMultiType(signed = false, Width2, Some(LittleEndian))
      case "u4le" => IntMultiType(signed = false, Width4, Some(LittleEndian))
      case "u8le" => IntMultiType(signed = false, Width8, Some(LittleEndian))
      case "u2be" => IntMultiType(signed = false, Width2, Some(BigEndian))
      case "u4be" => IntMultiType(signed = false, Width4, Some(BigEndian))
      case "u8be" => IntMultiType(signed = false, Width8, Some(BigEndian))
      case "s1" => Int1Type(signed = true)
      case "s2le" => IntMultiType(signed = true, Width2, Some(LittleEndian))
      case "s4le" => IntMultiType(signed = true, Width4, Some(LittleEndian))
      case "s8le" => IntMultiType(signed = true, Width8, Some(LittleEndian))
      case "s2be" => IntMultiType(signed = true, Width2, Some(BigEndian))
      case "s4be" => IntMultiType(signed = true, Width4, Some(BigEndian))
      case "s8be" => IntMultiType(signed = true, Width8, Some(BigEndian))
      case "f4le" => FloatMultiType(Width4, Some(LittleEndian))
      case "f8le" => FloatMultiType(Width8, Some(LittleEndian))
      case "f4be" => FloatMultiType(Width4, Some(BigEndian))
      case "f8be" => FloatMultiType(Width8, Some(BigEndian))
    }

  def singleTypeRef[$: P]: P[DataType] =
    P(primitiveTypeRef | Expressions.TYPE_NAME).map {
      case typeId: Ast.typeId => UserTypeInstream(typeId.names.toList, None, Seq())
      case dt: DataType => dt
    }

  def typeRef[$: P]: P[DataType] =
    P(singleTypeRef | singleTypeRef ~ "[" ~ "*".! ~ "]").map {
      case dt: DataType => dt
      case (dt: DataType, "*") => ArrayTypeInStream(dt)
    }

  def seqAttrSpec[$: P]: P[AttrSpec] =
    P(typeRef ~ Expressions.NAME ~~ (";" | "\n")).map{ case(dataType, name) =>
      AttrSpec(
        List(),
        NamedIdentifier(name.name),
        dataType
      )
    }

  def valueInstanceSpec[$: P]: P[ValueInstanceSpec] =
    P(Expressions.NAME ~ "=" ~ Expressions.test ~~ (";" | "\n")).map{ case(name, expr) =>
      ValueInstanceSpec(
        InstanceIdentifier(name.name),
        List(),
        expr
      )
    }

  def insideClassSpec[$: P]: P[YAMLPath] =
    P(classSpec | seqAttrSpec | valueInstanceSpec)

  def classSpec[$: P]: P[ClassSpec] =
    P("type" ~ Expressions.NAME ~/ "{" ~ insideClassSpec.rep ~ "}").map{
      case(id, members) =>
        val seqMembers: List[AttrSpec] = members.flatMap {
          case a: AttrSpec => Some(a)
          case _ => None
        }.toList

        val types: SortedMap[String, ClassSpec] = SortedMap.from(
          members.flatMap {
            case c: ClassSpec => Some((c.name.last, c))
            case _ => None
          }.toMap
        )

        val instances: SortedMap[InstanceIdentifier, InstanceSpec] = SortedMap.from(
          members.flatMap {
            case vis: ValueInstanceSpec => Some((vis.id, vis))
            case _ => None
          }
        )

        val thisType = ClassSpec(
          None,
          List(),
          isTopLevel = true,
          MetaSpec.OPAQUE,
          DocSpec.EMPTY,
          None,
          List(),
          seqMembers,
          types,
          instances,
          SortedMap()
        )
        thisType.name = List(id.name)

        thisType
    }

  def parse(src: String): ClassSpec = {
    val r = fastparse.parse(src.trim, classSpec(_), verboseFailures = true)
    r match {
      case Parsed.Success(value, _) => value
      case f: Parsed.Failure =>
        throw new RuntimeException(f.longMsg)
    }
  }
}
