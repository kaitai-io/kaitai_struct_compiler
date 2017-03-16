package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.format._
import io.kaitai.struct.precompile._
import io.kaitai.struct.translators._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TypeProcessor {
  def processTypesMany(specs: ClassSpecs, config: RuntimeConfig): Future[Unit] = {
    new LoadImports(specs).processClass(specs.firstSpec).map { (allSpecs) =>
      Log.importOps.info(() => s"imports done, got: ${specs.keys} (async=$allSpecs)")

      specs.foreach { case (_, classSpec) =>
        processTypes(specs, classSpec, config)
      }
    }
  }

  def processTypes(classSpecs: ClassSpecs, topClass: ClassSpec, config: RuntimeConfig): Unit = {
    classSpecs.foreach { case (_, curClass) => MarkupClassNames.markupClassNames(curClass) }
    val opaqueTypes = topClass.meta.get.opaqueTypes.getOrElse(config.opaqueTypes)
    new ResolveTypes(classSpecs, opaqueTypes).run()
    classSpecs.foreach { case (_, curClass) => ParentTypes.markup(curClass) }
    new SpecsValueTypeDerive(classSpecs).run()
    new TypeValidator(topClass).run()

    topClass.parentClass = GenericStructClassSpec
  }

  // ==================================================================

  def getOpaqueClasses(curClass: ClassSpec): Iterable[ClassSpec] = {
    val res = mutable.Set[ClassSpec]()
    curClass.seq.map((attr) =>
      res ++= getOpaqueDataTypes(attr.dataType)
    )
    curClass.instances.foreach { case (_, inst) =>
      inst match {
        case pis: ParseInstanceSpec =>
          res ++= getOpaqueDataTypes(pis.dataType)
        case _ => None
      }
    }

    // Traverse all nested types recursively
    curClass.types.foreach { case (_, nestedType) =>
      res ++= getOpaqueClasses(nestedType)
    }

    res
  }

  def getOpaqueDataTypes(dataType: DataType): Iterable[ClassSpec] = {
    dataType match {
      case ut: UserType =>
        if (ut.isOpaque) {
          List(ut.classSpec.get)
        } else {
          List()
        }
      case SwitchType(_, cases) =>
        cases.flatMap { case (_, ut) =>
          getOpaqueDataTypes(ut)
        }
      case _ =>
        // all other types are not opaque external user types
        List()
    }
  }
}
