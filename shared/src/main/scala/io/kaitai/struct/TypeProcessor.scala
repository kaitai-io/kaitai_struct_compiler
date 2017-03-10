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
  def processTypesMany(specs: ClassSpecs, firstSpec: ClassSpec, config: RuntimeConfig): Future[Unit] = {
    specs(firstSpec.name.head) = firstSpec
    new LoadImports(specs).processClass(firstSpec).map { (allSpecs) =>
      Log.importOps.info(() => s"imports done, got: ${specs.keys} (async=$allSpecs)")

      specs.foreach { case (_, classSpec) =>
        processTypes(specs, classSpec, config)
      }
    }
  }

  def processTypes(classSpecs: ClassSpecs, topClass: ClassSpec, config: RuntimeConfig): Unit = {
    classSpecs.foreach { case (_, curClass) => markupClassNames(curClass) }
    val opaqueTypes = topClass.meta.get.opaqueTypes.getOrElse(config.opaqueTypes)
    new ResolveTypes(classSpecs, opaqueTypes).run()
    classSpecs.foreach { case (_, curClass) => markupParentTypes(curClass) }
    new SpecsValueTypeDerive(classSpecs).run()
    new TypeValidator(topClass).run()

    topClass.parentClass = GenericStructClassSpec
  }

  // ==================================================================

  def markupClassNames(curClass: ClassSpec): Unit = {
    curClass.enums.foreach { case (enumName, enumSpec) =>
      enumSpec.name = curClass.name ::: List(enumName)
    }

    curClass.types.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = curClass.name ::: List(nestedName)
      nestedClass.upClass = Some(curClass)
      markupClassNames(nestedClass)
    }
  }

  // ==================================================================

  def markupParentTypes(curClass: ClassSpec): Unit = {
    Log.typeProcParent.info(() => s"markupParentTypes(${curClass.nameAsStr})")

    if (curClass.seq.nonEmpty)
      Log.typeProcParent.info(() => s"... seq")
    curClass.seq.foreach { attr =>
      markupParentTypesAdd(curClass, attr.dataType)
    }

    if (curClass.instances.nonEmpty)
      Log.typeProcParent.info(() => s"... instances")
    curClass.instances.foreach { case (_, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec =>
          markupParentTypesAdd(curClass, pis.dataTypeComposite)
        case _: ValueInstanceSpec =>
          // value instances have no effect on parenting, just do nothing
      }
    }
  }

  def markupParentTypesAdd(curClass: ClassSpec, dt: DataType): Unit = {
    dt match {
      case userType: UserType =>
        val parentClass = userType.forcedParent match {
          case None =>
            curClass
          case Some(parent) =>
            val provider = new ClassTypeProvider(curClass)
            val detector = new TypeDetector(provider)
            val parentType = detector.detectType(parent)
            Log.typeProcParent.info(() => s"..... enforced parent type = $parentType")
            parentType match {
              case ut: UserType =>
                ut.classSpec.get
              case other =>
                throw new TypeMismatchError(s"parent=$parent is expected to be of user type, but $other found")
            }
        }
        markupParentAs(parentClass, userType)
      case switchType: SwitchType =>
        switchType.cases.foreach {
          case (_, ut: UserType) =>
            markupParentAs(curClass, ut)
          case (_, _) =>
            // ignore everything else
        }
      case ArrayType(innerType) =>
        markupParentTypesAdd(curClass, innerType)
      case _ => // ignore, it's standard type
    }
  }

  def markupParentAs(curClass: ClassSpec, ut: UserType): Unit = {
    Log.typeProcParent.info(() => s"..... class=$ut has parent=${curClass.nameAsStr}")
    ut.classSpec match {
      case Some(usedClass) =>
        markupParentAs(curClass, usedClass)
      case None =>
        // TODO: replace with proper warning API
        Console.println(s"warning: tried to mark up parent=${curClass.name} for user type ${ut.name.mkString("::")}, but that type wasn't found, so doing nothing");
    }
  }

  def markupParentAs(parent: ClassSpec, child: ClassSpec): Unit = {
    child.parentClass match {
      case UnknownClassSpec =>
        child.parentClass = parent
        markupParentTypes(child)
      case otherClass: ClassSpec =>
        if (otherClass == parent) {
          // already done, don't do anything
        } else {
          // conflicting types, would be bad for statically typed languages
          // throw new RuntimeException(s"type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
          child.parentClass = GenericStructClassSpec
        }
      case GenericStructClassSpec =>
      // already most generic case, do nothing
    }
  }

  def getInstanceDataType(instSpec: InstanceSpec): DataType = {
    instSpec match {
      case t: ValueInstanceSpec => t.dataType.get
      case t: ParseInstanceSpec => t.dataTypeComposite
    }
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
