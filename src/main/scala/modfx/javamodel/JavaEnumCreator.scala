package modfx.javamodel

import modfx.model._

/**
  * Creates Java AST for enum
  * @param obj Mdx AST for enum
  */
class JavaEnumCreator(obj: Obj) extends JavaModelCreator {

  def create: Seq[JavaFile] = {
    val body = EnumBody(obj.nested.map{case EnumItem(itemName) => EnumEntry(itemName)}, Seq.empty)
    val enumDecl = EnumDecl(obj.name, implemented = Seq.empty, body)
    val pack = PackageDecl(obj.module + ".model")
    Seq(JavaFile(obj.name, ModelFile, pack, imports = Seq.empty, enumDecl))
  }
}
