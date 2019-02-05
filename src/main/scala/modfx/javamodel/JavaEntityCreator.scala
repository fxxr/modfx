package modfx.javamodel

import modfx.model._

/**
  * Creates Java AST for entity
  *
  */
class JavaEntityCreator(val entity: Entity) extends JavaModelCreator {
  private def meta = entity.meta
  private def fieldMeta = entity.fieldMeta
  private def innerMeta = entity.innerMeta
  private def subMeta = entity.subMeta

  def create: Seq[JavaFile] = {
    Seq(modelFile, metaFile)
  }
                          
  private def modelFile = {
    val body = Body(fieldDeclarations ++ Seq(cons, getMeta, getValue) ++ getInner ++ getValues)
    val classDecl = ClassDecl(entity.name, Seq(Public), Some(modelExtends), implemented = Seq.empty, body)
    val pack = PackageDecl(entity.module + ".model")
    JavaFile(entity.name, ModelFile, pack, modelImports, classDecl)
  }

  private def metaFile = {
    val declarations = fieldMetaDecl ++ innerMetaDecl ++ subMetaDecl ++ getFields :+ builder
    val body = EnumBody(EnumEntry(entity.metaInstance) :: Nil, declarations)
    val enumDecl = EnumDecl(entity.meta, Seq(Type("ObjectMeta.Entity", TypeParams(entity.name))), body)
    val pack = PackageDecl(entity.module + ".model.meta")
    JavaFile(entity.meta, MetaFile, pack, metaImports, enumDecl)
  }

  private def modelExtends = {
    entity.entityType match {
      case SimpleEntity => Type("Entity.Simple", TypeParams(meta, fieldMeta))
      case PlainEntity => Type("Entity.Plain", TypeParams(meta, fieldMeta, innerMeta))
      case GeneralEntity => Type("Entity", TypeParams(meta, fieldMeta, innerMeta, subMeta))
    }
  }

  private def fieldMetaDecl = {
    val entries = entity.nested.map(n => EnumEntry(n.name))
    val typeMethodBody = Block(
      Switch(
        NameExpr("this"),
        entity.nested.map(n => Case(n.name, Return("FieldType." + fieldType(n))))
      ),
      Return(MethodCall("throwNotImplemented"))
    )
    val typeMethod = MethodDecl(Type("FieldType"), "type", Seq(Public), overriding = true, Seq.empty, typeMethodBody)
    val methods = typeMethod +: refObject
    val fieldDecl = EnumDecl("Field", implemented = Type("FieldMeta")::Nil, EnumBody(entries, methods))
    fieldDecl :: Nil
  }

  private def fieldType(node: Nested) = node match {
    case f: Field => f.tpe
    case _ => "Nested"
  }

  private def refObject: Seq[MethodDecl] = entity.fields.filter(f => f.tpe == RefField) match {
    case Nil => Nil
    case refFields =>
      val cases = refFields.map(f => Case(f.name, Return(f.ref.map(r => metaInstName(r.name)).getOrElse("???"))))
      val body = Block(
        Switch(NameExpr("this"), cases),
        Return(MethodCall("throwNotImplemented"))
      )
      MethodDecl(Type("ObjectMeta", TypeParams("?")), "refObject", Seq(Public), overriding = true, Seq.empty, body) :: Nil
  }

  private def innerMetaDecl = entity.inners match {
    case Nil => Nil
    case innerFields: Seq[InnerField] =>
      val entries = innerFields.map(innerField => EnumEntry(innerField.name))
      val metaMethodType = Type("ObjectMeta", TypeParams("?"))
      val metaMethodBody = Block(
        Switch(
          NameExpr("this"),
          innerFields.map(innerField => Case(innerField.name, Return(metaInstName(innerField.inner.name))))
        ),
        Return(MethodCall("throwNotImplemented", NameExpr("this")))
      )
      val metaMethod = MethodDecl(metaMethodType, "meta", Seq(Public), overriding = true, Seq.empty, metaMethodBody)
      val innerDecl = EnumDecl("Inner", implemented = Type("InnerMeta")::Nil, EnumBody(entries, metaMethod :: Nil))
      innerDecl :: Nil
  }

  private def subMetaDecl = entity.subs match {
    case Nil => Nil
    case subFields: Seq[SubField] =>
      val entries = subFields.map(subField => EnumEntry(subField.name))
      val metaMethodType = Type("ObjectMeta", TypeParams("?"))
      val metaMethodBody = Block(
        Switch(
          NameExpr("this"),
          subFields.map(subField => Case(subField.name, Return(metaInstName(subField.sub.name))))
        ),
        Return(MethodCall("throwNoMetaForSub"))
      )
      val metaMethod = MethodDecl(metaMethodType, "meta", Seq(Public), overriding = true, Seq.empty, metaMethodBody)
      val refObjectMethodDecl = MethodDecl(
        metaMethodType, "refObject", Seq(Public), overriding = true,
        Seq.empty, Block(Return(entity.metaInstance))
      )
      val parentRefEnumBody = EnumBody(EnumEntry(entity.name) :: Nil, refObjectMethodDecl :: Nil)
      val parentRefEnum = EnumDecl("ParentRef", Seq(Type("FieldMeta.ParentRef")), parentRefEnumBody)
      val parentRefBody = Block(Return(s"ParentRef.${entity.name}"))
      val parentRefMethod = MethodDecl(Type("FieldMeta"), "parentRef", Seq(Public), overriding = true, Seq.empty, parentRefBody)
      val body = EnumBody(entries, metaMethod :: parentRefEnum :: parentRefMethod :: Nil)
      val subDecl = EnumDecl("Sub", implemented = Type("SubMeta") :: Nil, body)
      subDecl :: Nil
  }

  private def getFields = {
    val fieldsMethod = MethodDecl(
      Type("FieldMeta", TypeParams.empty, isArray = true),
      "fields", Seq(Public), overriding = true, params = Seq.empty,
      Block(Return("Field.values()"))
    )
    Seq(fieldsMethod)
  }

  private def builder = MethodDecl(
    Type("ObjectBuilder", TypeParams(entity.name, entity.meta)),
    "builder", Seq(Public), overriding = true, params = Seq.empty,
    Block(
    Return(
      MethodCall("ObjectBuilder", "of", Seq(
        entity.metaInstance,
        Func(Seq(Param(Type(""), "b")), ConstructorCall(
          entity.name,
          Seq(
            MethodCall("b", "getId", Seq.empty),
            MethodCall("b", "getGuid", Seq.empty)
          ) ++
          entity.nested.map(n => getFieldValueMethodCall(n))
        )))
      )
    ))
  )

  private def getFieldValueMethodCall(n: Nested): MethodCall = n match {
      case f: Field if f.tpe == EnumField =>
        MethodCall("b", "getEnumValue", Seq(
          "Field." + f.name, 
          f.refName + ".values()", 
          f.refName + "." + firstEnumElem(f.ref)
        ))
      case _ => MethodCall("b", getFieldName(n), Seq(nestedTypeName(n) + "." + n.name))
    }      
  
  def firstEnumElem(ref: Option[Obj]): String = ref match {
    case Some(e: Enum) => e.nested.headOption.map(_.name).getOrElse("???")
    case _ => "???"
  }

  private def getFieldName(nested: Nested): String = nested match {
    case _: SubField => "getSub"
    case _: InnerField => "getInner"
    case f: Field => "get" + f.tpe.toString
    case _ => "???"
  }

  private def nestedTypeName(nested: Nested): String = nested match {
    case _: SubField => "Sub"
    case _: InnerField => "Inner"
    case _: Field => "Field"
    case _ => "???"
  }

  private def fieldDeclarations: Seq[Declaration] = entity.nested map fieldDecl

  private def fieldDecl(nested: Nested): FieldDecl = FieldDecl(fType(nested), nested.fieldName, Seq(Public, Final))

  private def javaType(field: Field): String = field.tpe match {
    case StringField => "String"
    case IntField => "int"
    case ByteField => "byte"
    case ShortField => "short"
    case DateField => "OptDate"
    case DatetimeField => "OptDateTime"
    case BooleanField => "boolean"
    case MoneyField => "Money"
    case RefField | EnumField => field.ref.map(_.name).getOrElse("???")
  }
  
  private def fType(nested: Nested): Type = nested match {
    case f: Field => Type(javaType(f))
    case i: InnerField => Type(i.inner.name)
    case s: SubField => Type("List", TypeParams(s.sub.name))
  }
  
  private def cons: ConstructorDecl = {
    val params: Seq[Param] = Seq(
      Param(Type("int"), "id"),
      Param(Type("Guid"), "guid"),
    ) ++ entity.nested.map(f => Param(fType(f), f.fieldName))
    val statements: Seq[Statement] = 
      Super(NameExpr("id"), NameExpr("guid")) +:
      entity.nested.map(n => FieldAssign("this", n.fieldName, NameExpr(n.fieldName)))
    ConstructorDecl(entity.name, params, statements)
  }

  private def getMeta: Declaration = {
    val body = Block(Return(metaInstName(entity.name)))
    MethodDecl(Type(entity.meta), "meta", Seq(Public), overriding = true, Seq.empty, body)
  }

  private def getValue: Declaration = {
    val fieldParam = Seq(Param(Type(entity.fieldMeta), "field"))
    val cases = entity.nested map (n => Case(n.name, Return(n.fieldName)))
    val body = Block(
      Switch(NameExpr("field"), cases),
      Return(MethodCall("throwNotImplemented", NameExpr("field")))
    )
    MethodDecl(Type("Object"), "getValue", Seq(Public), overriding = true, fieldParam, body)
  }
  
  private def getInner: Seq[Declaration] = entity.inners match {
    case Nil => Nil
    case innerFields: Seq[InnerField] =>
      val cases = innerFields.map(innerField => Case(innerField.name, Return(innerField.fieldName)))
      val params = Seq(Param(Type(entity.innerMeta), "inner"))
      val body = Block(
        Switch(NameExpr("inner"), cases), 
        Return(MethodCall("throwNotImplemented", NameExpr("inner")))
      )
      MethodDecl(Type("Obj"), "getInner", Seq(Public), overriding = true, params, body) :: Nil
  }
  
  private def getValues: Seq[Declaration] = entity.subs match {
    case Nil => Nil
    case subFields: Seq[SubField] =>
      val cases = subFields.map(subField => Case(subField.name, Return(subField.fieldName)))
      val params = Seq(Param(Type(entity.subMeta), "sub"))
      val body = Block(
        Switch(NameExpr("sub"), cases), 
        Return(MethodCall("throwNotImplemented", NameExpr("sub")))
      )
      MethodDecl(Type("List", TypeParams("? extends Obj")), "getValues", Seq(Public), overriding = true, params, body) :: Nil
  }

  private def modelImports: Seq[ImportDecl] = {
    val metaFullName = entity.module + ".model.meta." + entity.meta
    val baseImports = Seq(
      ImportDecl("common.base.Guid", ProjectImport),
      ImportDecl("common.base.model.Entity", ProjectImport)
    )
    val metaImports = Seq(
      ImportDecl(metaFullName, ProjectImport),
      ImportDecl(metaFullName + "." + metaInstName(entity.name), StaticImport)
    )
    baseImports ++ metaImports
  }

  private def metaImports: Seq[ImportDecl] = {
    val baseImports = Seq(
      ImportDecl("common.base.model.ObjectMeta", ProjectImport),
      ImportDecl("common.base.model.FieldMeta", ProjectImport),
      ImportDecl("common.base.model.FieldType", ProjectImport),
      ImportDecl("common.base.model.ObjectBuilder", ProjectImport)
    )
    val modelPackagePrefix = entity.module + ".model."
    val modelImport = ImportDecl(modelPackagePrefix + entity.name, ProjectImport)
    val enumImports = entity.fields
      .filter(_.tpe == EnumField)
      .map(f => ImportDecl(modelPackagePrefix + f.refName, ProjectImport))
    def innerMetaImport = if (entity.inners.nonEmpty) 
      Seq(ImportDecl("common.base.model.InnerMeta", ProjectImport)) 
      else Seq.empty
    def subMetaImport = if (entity.subs.nonEmpty) Seq(ImportDecl("common.base.model.SubMeta", ProjectImport)) else Seq.empty
    (modelImport +: enumImports) ++ baseImports ++ innerMetaImport ++ subMetaImport
  }
}