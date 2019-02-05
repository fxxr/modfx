package modfx.javamodel

/**
  * Produces java code from AST
  */
object JavaModelFormatter {
  def toString(node: Node): String = toStrings(node, inner = false).mkString("\n")
  
  private def toStrings(node: Node): Seq[String] = toStrings(node, inner = true) 

  private def toStrings(node: Node, inner: Boolean): Seq[String] = node match {
    case JavaFile(_, _, pack, imports, mainType) => toStrings(pack) ++ toImportStrings(imports) ++ toStrings(mainType)
    case PackageDecl(name) => s"package $name;" :: "" :: Nil
    case ed: EnumDecl => enumDecl(ed, inner)
    case cd: ClassDecl => classDecl(cd)
    case fd: FieldDecl => toStr(fd) :: Nil
    case cons: ConstructorDecl => constructorDecl(cons)
    case md: MethodDecl => methodDecl(md)
    case ConstructorCall(name, params) => prependHead(s"new $name", callParamsChopDown(params))
    case Switch(expr, cases) => Seq(s"switch (${toStr(expr)}) {") ++ indent(cases map toStr) ++ Seq( "}")
    case Return(expr) => returnExpr(expr)
    case Func(params, body) => func(params, body)
    case MethodCall(ctx, name, params) => methodCall(ctx, name, params)
    case _ => toStr(node) :: Nil
  }

  private def toStr(node: Node): String = node match {
    case ImportDecl(name, StaticImport) => s"import static $name;"
    case ImportDecl(name, _) => s"import $name;"
    case Type(name, typeParams, isArray) => name + toStr(typeParams) + (if (isArray) "[]" else "")
    case TypeParams(params @ _*) => if (params.nonEmpty) s"<${params.mkString(", ")}>" else ""
    case EnumEntry(name) => name
    case FieldDecl(tpe, name, modifiers) =>
      val mod = modifiers.map(modifier).mkString(" ")
      s"$mod ${toStr(tpe)} $name;"
    case Param(tpe, name) => s"${toStr(tpe)} $name"
    case Super(params) => s"super${callParams(params).mkString(", ")};"
    case FieldAssign(ctx, name, expr) => s"$ctx.$name = ${toStr(expr)};"
    case NameExpr(name) => name
    case Case(name, statements) => s"case $name: " + statements.map(toStr).mkString(" ")
    case Return(expr) => expr match {
      case Some(e) => s"return ${toStr(e)};"
      case None => "return;"
    }
    case _ => s"?? $node ??"
  }

  private def func(params: Params, body: Expression): Seq[String] = {
    val funcParams = params match {
      case Nil => "()"
      case singleParam :: Nil => singleParam.name
      case _ => "(" + params.map(_.name).mkString(", ") + ")"
    }
    prependHead(s"$funcParams -> ", toStrings(body)) 
  }
  
  private[this] def prependHead(prefix: String, coll: Seq[String]): Seq[String] = coll match {
    case Nil => prefix :: Nil
    case head :: tail => (prefix + head) +: tail
  }
  
  private def methodCall(ctx: String, name: Name, params: CallParams): Seq[String] = {
    val methodName = if (ctx.isEmpty) name else ctx + "." + name
    prependHead(methodName, callParamsChopDown(params))
  }
  
  private def returnExpr(expr: Option[Expression]): Seq[String] = expr.map(toStrings).getOrElse(Nil) match {
    case Nil => "return;" :: Nil
    case head :: Nil => s"return $head;" :: Nil
    case head :: tail => s"return $head" +: appendSemicolon(tail)
  }
  
  private def appendSemicolon(strings: Seq[String]): Seq[String] = strings match {
    case Nil => Nil
    case single :: Nil => single + ";" :: Nil
    case _ =>
      val reversed = strings.reverse
      ((reversed.head + ";") +: reversed.tail).reverse
  }
  
  private def toImportStrings(imports: Seq[ImportDecl]): Seq[String] = imports match {
    case Nil => Nil
    case _ => (imports map toStr) :+ ""
  }

  private def callParams(params: CallParams): Seq[String] = params.map(toStrings) match {
    case Nil => "()" :: Nil
    case fewParams if fewFlatParams(fewParams) => "(" + fewParams.map(_.head).mkString(", ") + ")" :: Nil
    case moreParams => "(" +: indent(moreParams.flatten) :+ ")"
  }

  private def fewFlatParams(fewParams: Seq[Seq[String]]): Boolean = 
    fewParams.map(_.length).sum <= 3 && fewParams.forall(_.length == 1)
  
  private def callParamsChopDown(params: CallParams): Seq[String] = params match {
    case Nil => "()" :: Nil
    case oneParam :: Nil => Seq("(" + toStr(oneParam) + ")")
    case _ => "(" +: chopDownParams(params) :+ ")"
  }

  private def appendCommas(strings: Seq[Seq[String]]): Seq[String] = {
    strings.init.flatMap(i => i.init :+ i.last + ",") ++ strings.last
  }

  private def chopDownParams(params: CallParams): Seq[String] = appendCommas(params.map(p => indent(toStrings(p))))

  private def enumDecl(enumDecl: EnumDecl, inner: Boolean): Seq[String] = {
    val implementsPart = join(" implements ", enumDecl.implemented)
    val res = (s"public enum ${enumDecl.name}$implementsPart {" :: Nil) ++ indent(enumBody(enumDecl.body)) :+ "}"
    if (inner) "" +: res else res
  }

  private def classDecl(classDecl: ClassDecl): Seq[String] = {
    val impl = join(" implements ", classDecl.implemented)
    val ext = classDecl.extended.map(tpe => " extends " + toStr(tpe)).getOrElse("???")
    val mod = classDecl.modifiers.map(modifier).mkString(" ")
    (s"$mod class ${classDecl.name}$ext$impl {" :: Nil) ++ indent(body(classDecl.body)) :+ "}"
  }
  
  private def constructorDecl(cons: ConstructorDecl): Seq[String] = {
    val consLines = cons.statements map toStr
    ("" :: s"public ${cons.name}(${cons.params.map(toStr).mkString(", ")}) {" :: Nil ) ++ indent(consLines) :+ "}"
  }

  private def methodDecl(methodDecl: MethodDecl): Seq[String] = {
    val mod = methodDecl.modifiers.map(modifier).mkString(" ")
    val tpe = toStr(methodDecl.tpe)
    val methodLines = methodDecl.body.statements.flatMap(toStrings)
    val params = methodDecl.params.map(toStr).mkString(", ")
    val prefix = if (methodDecl.overriding) "" :: "@Override" :: Nil else "" :: Nil
    prefix ++ (s"$mod $tpe ${methodDecl.name}($params) {" :: Nil) ++ indent(methodLines) :+ "}"
  }
  
  private def enumBody(enumBody: EnumBody): Seq[String] = {
    val declarations = enumBody.enumDeclarations.flatMap(toStrings)
    val entries = declarations match {
      case Nil => enumBody.entries.init.map(_.name + ",") :+ s"${enumBody.entries.last.name}"
      case _ => enumBody.entries.init.map(_.name + ",") :+ s"${enumBody.entries.last.name};"
    }
    entries ++ declarations
  }

  private def body(body: Body): Seq[String] = body.declarations.flatMap(toStrings)

  private def modifier(modifier: Modifier): String = modifier match {
    case Public => "public"
    case Private => "private"
    case Static => "static"
    case Final => "final"
  }

  private def join[N <: Node](prefix: String, list: Seq[N], sep: String = ", "): String = list match {
    case Nil => ""
    case nel: Seq[N] => prefix + nel.map(toStr).mkString(", ")
  }

  private def indent(str: String): String = "    " + str
  private def indent(strings: Seq[String]): Seq[String] = strings map indent
}
