package modfx.javamodel

import modfx.model._

/**
  * Creates Java AST for entity
  *
  */
class JavaEntityCreator(val entity: Entity) extends JavaModelCreator(entity) with InnerSupport {
  override def create: List[JavaFile] = List(modelFile, metaFile)

  import modelattrs._

  override val basic: BasicObj = entity

  private def modelFile = {
    val body = Body(
      fieldDeclarations ++
        innerClasses ++
        Seq(
          cons,
          getMeta,
          getValue
        ) ++
        getInner ++
        getValues
    )
    val classDecl = ClassDecl(entity.name, List(Public), Some(modelExtends), implemented = List.empty, body)
    val pack = PackageDecl(entity.module + ".model")
    JavaFile(entity.name, ModelFile, pack, modelImports, classDecl)
  }

  private def metaFile = {
    val pack = PackageDecl(entity.module + ".model.meta")
    JavaFile(entity.meta, MetaFile, pack, metaImports, metaEnumDecl)
  }
  
  def metaEnumDecl: EnumDecl = {
    val declarations = fieldMetaDecl ++ innerMetaDecl ++ subMetaDecl ++ getFields :+ builder
    val innerMetas = innersWithin.map(i => new JavaInnerCreator(i.inner).metaEnumDecl)
    val body = EnumBody(EnumEntry(entity.metaInstance) :: Nil, innerMetas ++ declarations)
    EnumDecl(entity.meta, List(Type("ObjectMeta.Entity", TypeParams(entity.name))), body)
  }

  private def modelExtends = {
    val meta = entity.meta
    val fieldMeta = entity.fieldMeta
    val innerMeta = entity.innerMeta
    val subMeta = entity.subMeta
    entity.nestingLevel match {
      case PlainNesting => Type("Entity.Plain", TypeParams(meta, fieldMeta))
      case BasicNesting => Type("Entity.Basic", TypeParams(meta, fieldMeta, innerMeta))
      case GeneralNesting => Type("Entity", TypeParams(meta, fieldMeta, innerMeta, subMeta))
    }
  }

  private def subMetaDecl = entity.subs match {
    case Nil => Nil
    case subFields: List[SubField] =>
      val entries = subFields.map(subField => EnumEntry(subField.name))
      val metaMethodType = Type("ObjectMeta", TypeParams("?"))
      val metaMethodBody = Block(
        Switch(
          NameExpr("this"),
          subFields.map(subField => Case(subField.name, Return(subField.sub.metaInstance)))
        ),
        Return(MethodCall("throwNoMetaForSub"))
      )
      val metaMethod = MethodDecl(metaMethodType, "meta", List(Public), overriding = true, List.empty, metaMethodBody)
      val refObjectMethodDecl = MethodDecl(
        metaMethodType, "refObject", List(Public), overriding = true,
        List.empty, Block(Return(entity.metaInstance))
      )
      val parentRefEnumBody = EnumBody(EnumEntry(entity.name) :: Nil, refObjectMethodDecl :: Nil)
      val parentRefEnum = EnumDecl("ParentRef", List(Type("FieldMeta.ParentRef")), parentRefEnumBody)
      val parentRefBody = Block(Return(s"ParentRef.${entity.name}"))
      val parentRefMethod = MethodDecl(Type("FieldMeta"), "parentRef", List(Public), overriding = true, List.empty, parentRefBody)
      val body = EnumBody(entries, metaMethod :: parentRefEnum :: parentRefMethod :: Nil)
      val subDecl = EnumDecl("Sub", implemented = Type("SubMeta") :: Nil, body)
      subDecl :: Nil
  }

  private def builder = MethodDecl(
    Type("ObjectBuilder", TypeParams(entity.name, entity.meta)),
    "builder", List(Public), overriding = true, params = List.empty,
    Block(
      Return(
        MethodCall("ObjectBuilder", "of", List(
          entity.metaInstance,
          Func(List(Param(Type(""), "b")), ConstructorCall(
            entity.name,
            List(
              MethodCall("b", "getId", List.empty),
              MethodCall("b", "getGuid", List.empty)
            ) ++
            entity.nested.map(n => getFieldValueMethodCall(n))
          )))
        )
      )
    )
  )

  private def cons: ConstructorDecl = {
    val params: List[Param] = List(
      Param(Type("int"), "id"),
      Param(Type("Guid"), "guid"),
    ) ++ entity.nested.map(f => Param(fType(f), f.fieldName))
    val statements: List[Statement] =
      Super(NameExpr("id"), NameExpr("guid")) +:
      entity.nested.map(n => FieldAssign("this", n.fieldName, NameExpr(n.fieldName)))
    ConstructorDecl(entity.name, params, statements)
  }

  private def getValues: List[Declaration] = entity.subs match {
    case Nil => Nil
    case subFields: List[SubField] =>
      val cases = subFields.map(subField => Case(subField.name, Return(subField.fieldName)))
      val params = List(Param(Type(entity.subMeta), "sub"))
      val body = Block(
        Switch(NameExpr("sub"), cases),
        Return(MethodCall("throwNotImplemented", NameExpr("sub")))
      )
      MethodDecl(Type("List", TypeParams("? extends Obj")), "getValues", List(Public), overriding = true, params, body) :: Nil
  }

  private def modelImports: List[ImportDecl] = {
    val metaFullName = entity.module + ".model.meta." + entity.meta
    val baseInnerImports =
      if (innersWithin.nonEmpty) List(ImportDecl("common.base.model.InnerObject", ProjectImport)) else Nil
    val baseImports = List(
      ImportDecl("common.base.Guid", ProjectImport),
      ImportDecl("common.base.model.Entity", ProjectImport)
    ) ++ baseInnerImports
    val metaImports = List(
      ImportDecl(metaFullName, ProjectImport),
      ImportDecl(metaFullName + "." + entity.metaInstance, StaticImport)
    ) ++ metaInstInnerImports
    val subImports = if (entity.subs.nonEmpty) List(ImportDecl("java.util.List", StdImport)) else Nil
    val innerFieldTypeImports: List[ImportDecl] = innersWithin.flatMap(i => fieldTypeImports(i.inner))
    val fTypeImports = fieldTypeImports() ++ innerFieldTypeImports
    (baseImports ++ fTypeImports ++ objImport ++ refImports ++ subImports ++ metaImports).distinct
  }

  private def metaInstImport(obj: Obj): ImportDecl =
    ImportDecl(
      s"${obj.module}.model.meta.${containerMetaPrefix(obj)}${obj.name}Meta.${obj.metaInstance}",
      StaticImport
    )

  private def metaInstInnerImports: List[ImportDecl] = innersWithin.map(_.inner).map(metaInstImport)

  private def metaImports: List[ImportDecl] = {
    val subMetaImport = if (entity.subs.nonEmpty) List(ImportDecl("common.base.model.SubMeta", ProjectImport)) else List.empty
    val metaSubImports = entity.subs.map(_.sub).map(metaInstImport)
    val metaInstImports = metaRefImports ++ metaInnerImports ++ metaSubImports
    val innerFieldTypeImports: List[ImportDecl] = innersWithin.flatMap(i => fieldTypeImports(i.inner))
    val innerInnerMetaImports = innersWithin.flatMap(i => i.inner.inners).map(i => metaInstImport(i.inner))

    ((modelImport +: enumImports) ++ metaBaseImports ++
      innerMetaImport ++ innerFieldTypeImports ++ innerInnerMetaImports ++ subMetaImport ++ metaInstImports).distinct
  }
}