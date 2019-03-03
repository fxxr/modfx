package modfx.javamodel

import modfx.model._

class JavaInnerCreator(val inner: Inner) extends JavaModelCreator(inner) with InnerSupport {
  override def create: List[JavaFile] = List(modelFile, metaFile)

  import modelattrs._

  override val basic: BasicObj = inner

  private def modelFile = {
    val pack = PackageDecl(inner.module + ".model")
    JavaFile(inner.name, ModelFile, pack, modelImports, modelClassDecl)
  }

  private def metaFile = {
    val pack = PackageDecl(inner.module + ".model.meta")
    JavaFile(inner.meta, MetaFile, pack, metaImports, metaEnumDecl)
  }
  
  def metaEnumDecl: EnumDecl = {
    val declarations = fieldMetaDecl ++ innerMetaDecl ++ getFields :+ builder
    val body = EnumBody(EnumEntry(inner.metaInstance) :: Nil, declarations)
    EnumDecl(inner.meta, List(Type("ObjectMeta.Inner", TypeParams(containerPrefixedName(inner)))), body)
  }

  def modelClassDecl: ClassDecl = {
    val containerPrefix = inner.container.map(c => c + "Meta.").getOrElse("")
    val body = Body(
      fieldDeclarations ++
        innerClasses ++
        List(
          cons,
          getMeta,
          getValue
        ) ++
        getInner
    )
    val modifiers = if (inner.container.isDefined) List(Public, Static) else List(Public)
    ClassDecl(inner.name, modifiers, None, implemented = List(modelImplements(containerPrefix)), body)
  }

  private def modelImplements(containerPrefix: String) = {
    val meta = containerPrefix + inner.meta
    val fieldMeta = containerPrefix + inner.fieldMeta
    val innerMeta = containerPrefix + inner.innerMeta
    inner.nestingLevel match {
      case PlainNesting => Type("InnerObject.Plain", TypeParams(meta, fieldMeta))
      case BasicNesting => Type("InnerObject", TypeParams(meta, fieldMeta, innerMeta))
    }
  }

  private def containerPrefixedName(inner: Inner): String = containerPrefix(inner) + inner.name

  private def builder = MethodDecl(
    Type("ObjectBuilder", TypeParams(containerPrefixedName(inner), inner.meta)),
    "builder", List(Public), overriding = true, params = List.empty,
    Block(
      Return(
        MethodCall("ObjectBuilder", "of", List(
          inner.metaInstance,
          Func(
            List(Param(name = "b")),
            ConstructorCall(containerPrefixedName(inner), inner.nested.map(n => getFieldValueMethodCall(n)))
          ))
        )
      )
    )
  )

  private def cons: ConstructorDecl = {
    val params: List[Param] = inner.nested.map(f => Param(fType(f), f.fieldName))
    val statements: List[Statement] =
        inner.nested.map(n => FieldAssign("this", n.fieldName, NameExpr(n.fieldName)))
    ConstructorDecl(inner.name, params, statements)
  }

  private def modelImports: List[ImportDecl] = {
    val metaFullName = inner.module + ".model.meta." + inner.meta
    val baseImports = List(ImportDecl("common.base.model.InnerObject", ProjectImport))
    val metaImports = List(
      ImportDecl(metaFullName, ProjectImport),
      ImportDecl(metaFullName + "." + inner.metaInstance, StaticImport)
    )
    baseImports ++ fieldTypeImports() ++ objImport ++ refImports ++ metaImports
  }

  private def metaImports: List[ImportDecl] = 
    (modelImport +: enumImports) ++ metaBaseImports ++ innerMetaImport ++ metaRefImports ++ metaInnerImports
}
