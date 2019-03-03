package modfx.javamodel

import modfx.javamodel.JavaModelCreator.{containerMetaPrefix, metaInstImport}
import modfx.model._

/**
  * Creates java models from an intermediate object model
  */
object JavaModelCreator {

  def toJavaModel(obj: Obj): List[JavaFile] = obj match {
    case _: Enum => new JavaEnumCreator(obj).create
    case entity: Entity => new JavaEntityCreator(entity).create
    case inner: Inner => new JavaInnerCreator(inner).create
    case _ => Nil
  }

  import modelattrs._

   def metaInstImport(obj: Obj): ImportDecl =
     ImportDecl(s"${obj.module}.model.meta.${obj.name}Meta.${obj.metaInstance}", StaticImport)

  def containerMetaPrefix(obj: Obj): String = obj match {
    case inner: Inner => inner.container.map(_ + "Meta.").getOrElse("")
    case _ => ""
  }

  def containerName(obj: Obj): String = obj match {
    case Inner(_, _, _, Some(container)) => container
    case _ => ""
  }
}

trait InnerSupport {
  val basic: BasicObj

  import modelattrs._

  def containerPrefix(obj: Obj): String = suffix(JavaModelCreator.containerName(obj), ".")

  def containerMetaPrefix(obj: Obj): String = suffix(JavaModelCreator.containerName(obj), "Meta.")

  def suffix(str: String, suffix: String): String = if (str.isEmpty) "" else str + suffix

  protected def innersWithin: List[InnerField] = basic.inners.filter(_.inner.container.isDefined)

  protected def innerClasses: List[ClassDecl] =
    innersWithin.map(i => new JavaInnerCreator(i.inner).modelClassDecl)

  protected def innerMetaDecl: List[EnumDecl] = basic.inners match {
      case Nil => Nil
      case innerFields: List[InnerField] =>
        val entries = innerFields.map(innerField => EnumEntry(innerField.name))
        val metaMethodType = Type("ObjectMeta", TypeParams("?"))
        def innerMetaInst(inner: Inner): String = inner.container match {
          case Some(_) => inner.name + "Meta." + inner.metaInstance
          case None => inner.metaInstance
        }
        val metaMethodBody = Block(
          Switch(
            NameExpr("this"),
            innerFields.map(innerField =>
              Case(innerField.name, Return(innerMetaInst(innerField.inner)))
            )
          ),
          Return(MethodCall("throwNotImplemented", NameExpr("this")))
        )
        val metaMethod = MethodDecl(metaMethodType, "meta", List(Public), overriding = true, List.empty, metaMethodBody)
        val innerDecl = EnumDecl("Inner", implemented = Type("InnerMeta")::Nil, EnumBody(entries, metaMethod :: Nil))
        innerDecl :: Nil
    }

  protected def getInner: List[Declaration] = basic.inners match {
    case Nil => Nil
    case innerFields: List[InnerField] =>
      val cases = innerFields.map(innerField => Case(innerField.name, Return(innerField.fieldName)))
      val params = List(Param(Type(containerMetaPrefix(basic) + basic.innerMeta), "inner"))
      val body = Block(
        Switch(NameExpr("inner"), cases),
        Return(MethodCall("throwNotImplemented", NameExpr("inner")))
      )
      MethodDecl(Type("Obj"), "getInner", List(Public), overriding = true, params, body) :: Nil
  }

  protected def objImport: List[ImportDecl] =
    if (basic.inners.nonEmpty) List(ImportDecl("common.base.model.Obj", ProjectImport)) else Nil

  protected def metaInnerImports: List[ImportDecl] = basic.inners.map(_.inner).filter(_.container.isEmpty).map(metaInstImport)

  protected def innerMetaImport: List[ImportDecl] = if (basic.inners.nonEmpty)
    List(ImportDecl("common.base.model.InnerMeta", ProjectImport))
    else List.empty
}

abstract class JavaModelCreator(obj: Obj) {
  def create: List[JavaFile]

  import modelattrs._

  protected def fieldMetaDecl: List[EnumDecl] = {
    val entries = obj.nested.map(n => EnumEntry(n.name))
    val typeMethodBody = Block(
      Switch(
        NameExpr("this"),
        obj.nested.map(n => Case(n.name, Return("FieldType." + fieldType(n))))
      ),
      Return(MethodCall("throwNotImplemented"))
    )
    val typeMethod = MethodDecl(Type("FieldType"), "type", List(Public), overriding = true, List.empty, typeMethodBody)
    val methods = (typeMethod +: sizeMethod) ++ refObject
    val fieldDecl = EnumDecl("Field", implemented = Type("FieldMeta") :: Nil, EnumBody(entries, methods))
    fieldDecl :: Nil
  }

  private def fieldType(node: Nested) = node match {
    case f: Field => f.tpe
    case _ => "Nested"
  }

  protected def getFieldValueMethodCall(n: Nested): MethodCall = n match {
      case f: Field if f.tpe == EnumField =>
        MethodCall("b", "getEnumValue", List(
          "Field." + f.name,
          f.refName + ".values()",
          f.refName + "." + firstEnumElem(f.ref)
        ))
      case _ => MethodCall("b", getFieldName(n), List(nestedTypeName(n) + "." + n.name))
    }

  private def firstEnumElem(ref: Option[Obj]): String = ref match {
    case Some(e: Enum) => e.nested.headOption.map(_.name).getOrElse("???")
    case _ => "???"
  }

  protected def refFields: List[RefField] = obj.fields.filter(f => f.tpe == RefField).flatMap {
    case Field(name, _, _, Some(ref)) => List(RefField(name, ref))
  }

  protected def refObjects: List[Obj] = refFields.map(_.ref)

  protected def fold[T](coll: List[T]): List[List[T]] = if (coll.isEmpty) Nil else List(coll)

  protected def refObject: List[MethodDecl] =
    fold(refFields).flatMap { refFields =>
      val cases = refFields.map(f => Case(f.name, Return(f.ref.metaInstance)))
      val body = Block(
        Switch(NameExpr("this"), cases),
        Return(MethodCall("throwNotImplemented"))
      )
      MethodDecl(Type("ObjectMeta", TypeParams("?")), "refObject", List(Public), overriding = true, List.empty, body) :: Nil
    }

  private def sizeMethod: List[MethodDecl] = {
    val fieldsWithSize = obj.fields.filter(_.size > 0)
    fieldsWithSize match {
      case Nil => Nil
      case fields => List(MethodDecl(
        Type("int"),
        "size",
        List(Public), overriding = true, Nil,
        Block(
          Switch(NameExpr("this"), fields.map(f => Case(f.name, Return(IntExpr(f.size))))),
          Return(IntExpr(255))
        )
      ))
    }
  }

  protected def getValue: Declaration = {
    val fieldParam = List(Param(Type(containerMetaPrefix(obj) + obj.fieldMeta), "field"))
    val cases = obj.nested map (n => Case(n.name, Return(n.fieldName)))
    val body = Block(
      Switch(NameExpr("field"), cases),
      Return(MethodCall("throwNotImplemented", NameExpr("field")))
    )
    MethodDecl(Type("Object"), "getValue", List(Public), overriding = true, fieldParam, body)
  }

  protected def getMeta: Declaration = {
    val body = Block(Return(obj.metaInstance))
    MethodDecl(Type(containerMetaPrefix(obj) + obj.meta), "meta", List(Public), overriding = true, List.empty, body)
  }

  protected def getFields: List[MethodDecl] = {
    val fieldsMethod = MethodDecl(
      Type("FieldMeta", TypeParams.empty, isArray = true),
      "fields", List(Public), overriding = true, params = List.empty,
      Block(Return("Field.values()"))
    )
    List(fieldsMethod)
  }

  protected def getFieldName(nested: Nested): String = nested match {
    case _: SubField => "getSub"
    case _: InnerField => "getInner"
    case f: Field if f.ref.isDefined => "getRefObject"
    case f: Field => "get" + f.tpe.toString
    case _ => "???"
  }

  protected def nestedTypeName(nested: Nested): String = nested match {
    case _: SubField => "Sub"
    case _: InnerField => "Inner"
    case _: Field => "Field"
    case _ => "???"
  }

  protected def fieldDeclarations: List[Declaration] = obj.nested.map(fieldDecl)

  protected def fieldDecl(nested: Nested): FieldDecl = FieldDecl(fType(nested), nested.fieldName, List(Public, Final))

  protected def javaType(field: Field): String = field.tpe match {
    case StringField => "String"
    case IntField => "int"
    case ByteField => "byte"
    case ShortField => "short"
    case DateField => "OptDate"
    case DateTimeField => "OptDateTime"
    case BooleanField => "boolean"
    case MoneyField => "Money"
    case RefField | EnumField => field.ref.map(_.name).getOrElse("??")
  }

  protected def fType(nested: Nested): Type = nested match {
    case f: Field => Type(javaType(f))
    case i: InnerField => Type(i.inner.name)
    case s: SubField => Type("List", TypeParams(s.sub.name))
  }
  
  protected def fieldTypeImports(o: Obj = obj): List[ImportDecl] = 
    o.fields.flatMap(typeImport).map(i => ImportDecl(i, ProjectImport))

  protected def refImports: List[ImportDecl] = refObjects.filter(_.module != obj.module)
    .map(ref => ImportDecl(s"${ref.module}.model.${ref.name}", ProjectImport))

  private def typeImport(field: Field): List[String] = field.tpe match {
    case DateField => List("common.date.OptDate")
    case DateTimeField => List("common.date.OptDateTime")
    case MoneyField => List("common.base.Money")
    case EnumField => field.ref.map(ref => ref.module + ".model." + ref.name).toList
    case _ => Nil
  }

  protected def metaBaseImports: List[ImportDecl] = List(
    ImportDecl("common.base.model.ObjectMeta", ProjectImport),
    ImportDecl("common.base.model.FieldMeta", ProjectImport),
    ImportDecl("common.base.model.FieldType", ProjectImport),
    ImportDecl("common.base.model.ObjectBuilder", ProjectImport)
  )

  private def modelPackagePrefix = obj.module + ".model."
  
  protected def modelImport: ImportDecl = ImportDecl(modelPackagePrefix + obj.name, ProjectImport)
  
  protected def enumImports: List[ImportDecl] = obj.fields
    .filter(_.tpe == EnumField)
    .map(f => ImportDecl(modelPackagePrefix + f.refName, ProjectImport))
  
  protected def metaRefImports: List[ImportDecl] = refObjects.map(metaInstImport)
} 