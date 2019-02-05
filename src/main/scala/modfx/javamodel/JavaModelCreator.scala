package modfx.javamodel

import modfx.model._

/**
  * Creates java models from an intermediate object model
  */
object JavaModelCreator {

  def toJavaModel(obj: Obj): Seq[JavaFile] = obj match {
    case _: Enum => new JavaEnumCreator(obj).create
    case entity: Entity => new JavaEntityCreator(entity).create
  }
}

trait JavaModelCreator {
  def create: Seq[JavaFile]
}
