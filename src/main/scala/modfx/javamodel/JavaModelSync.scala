package modfx.javamodel

import modfx.model._
import better.files.Dsl.SymbolicOperations
import better.files._

/**
  * Outputs generated java code to files
  * @param moduleRoot The root folder for models
  * @param objs Mdx object AST's
  */

class JavaModelSync(val moduleRoot: File, val objs: Seq[Obj]) {
  
  def run(): Unit = objs.foreach(sync)

  private def sync(obj: Obj): Unit = {
    val files = JavaModelCreator.toJavaModel(obj)
    files foreach (f => {
      val javaCode = JavaModelFormatter.toString(f)
      val pack = f.tpe match {
        case ModelFile => "model"
        case MetaFile => "model/meta"
      }
      val javaFile = moduleRoot / pack / (f.name + ".java")
      javaFile.createIfNotExists(createParents = true)
      javaFile < javaCode
      println(javaFile.pathAsString)
    })
  }
}
