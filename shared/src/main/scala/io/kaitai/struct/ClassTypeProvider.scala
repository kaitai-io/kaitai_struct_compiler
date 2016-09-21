package io.kaitai.struct

import io.kaitai.struct.exprlang.DataType.{BaseType, KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.format._
import io.kaitai.struct.translators.TypeProvider

class ClassTypeProvider(topClass: ClassSpec) extends TypeProvider {
  var nowClass = topClass

  var _currentIteratorType: Option[BaseType] = None
  var _currentSwitchType: Option[BaseType] = None
  def currentIteratorType: BaseType = _currentIteratorType.get
  def currentSwitchType: BaseType = _currentSwitchType.get

  override def determineType(attrName: String): BaseType = {
    Console.println(s"determineType($attrName), nowClass=${nowClass.name}, nowClass.parent=${nowClass.parentTypeName}")
    determineType(nowClass, attrName)
  }

  override def determineType(inClass: ClassSpec, attrName: String): BaseType = {
    attrName match {
      case "_root" =>
        makeUserType(topClass)
      case "_parent" =>
        makeUserType(inClass.parentClass)
      case "_io" =>
        KaitaiStreamType
      case "_" =>
        currentIteratorType
      case "_on" =>
        currentSwitchType
      case _ =>
        inClass.seq.foreach { el =>
          if (el.id == NamedIdentifier(attrName))
            return el.dataTypeComposite
        }
        inClass.instances.get(InstanceIdentifier(attrName)) match {
          case Some(i: ValueInstanceSpec) => return i.dataType.get
          case Some(i: ParseInstanceSpec) => return i.dataTypeComposite
          case None => // do nothing
        }
        throw new RuntimeException(s"Unable to access $attrName in ${inClass.name} context")
    }
  }

  def makeUserType(csl: ClassSpecLike): UserTypeInstream = {
    csl match {
      case GenericStructClassSpec =>
        UserTypeInstream(List("kaitai_struct"))
      case cs: ClassSpec =>
        val ut = UserTypeInstream(cs.name)
        ut.classSpec = Some(cs)
        ut
    }
  }
}
