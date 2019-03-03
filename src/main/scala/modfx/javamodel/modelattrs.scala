package modfx.javamodel

import modfx.model.Obj


object modelattrs {
  implicit class ModelAttrs(obj: Obj) {
    val meta: String = obj.name + "Meta"
    val metaInstance: String = metaInstName(obj.name)
    val fieldMeta: String = meta + ".Field"
    val innerMeta: String = meta + ".Inner"
    val subMeta: String = meta + ".Sub"
  }

  def splitCamelCase(str: String): Seq[String] = str.split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])")

  def metaInstName(name: String): String = splitCamelCase(name).map(_.toUpperCase).mkString("_")
}
