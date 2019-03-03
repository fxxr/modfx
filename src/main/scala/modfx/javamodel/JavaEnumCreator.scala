package modfx.javamodel

import modfx.model._

/**
  * Creates Java AST for enum
  * @param obj Mdx AST for enum
  */
class JavaEnumCreator(obj: Obj) extends JavaModelCreator(obj) {

  def create: List[JavaFile] = {
    val body = EnumBody(obj.nested.map{case EnumItem(itemName) => EnumEntry(itemName)}, List.empty)
    val enumDecl = EnumDecl(obj.name, implemented = List.empty, body)
    val pack = PackageDecl(obj.module + ".model")
    List(JavaFile(obj.name, ModelFile, pack, imports = List.empty, enumDecl))
  }
}
