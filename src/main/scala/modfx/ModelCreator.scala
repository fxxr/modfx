package modfx

import modfx.MdxParser.Ast
import modfx.model._

/**
  * Creates an intermediate model from mdx AST
  * @param topNodes AST's of mdx's top nodes
  * @param module Module name
  */

class ModelCreator(val topNodes: Seq[Ast.TopNode], val module: String) {
  sealed trait ObjType
  object ObjType {
    case object Entity extends ObjType
    case object Inner extends ObjType
    case object Sub extends ObjType
    case object Base extends ObjType
    case object Enum extends ObjType
  }

  def create(): Seq[Obj] = {
    val objTypes: Map[String, ObjType] = topNodes.map(n => n.name -> decodeObjType(n.descriptor)).toMap
    def collectNodes(node: Ast.Nesting): Seq[(String, Ast.Nesting)] = 
      (node.nestingName -> node) +: node.nesting.filter(_.descriptor.isDefined).flatMap(collectNodes)
    val nodeMap: Map[String, Ast.Nesting] = topNodes.flatMap(collectNodes).toMap
    topNodes.filter(isRootObject).map(n => createObj(n.name, n, new Context(objTypes, nodeMap, createObj)))
  }

  def isRootObject(node: Ast.TopNode): Boolean = {
    val tpe = decodeObjType(node.descriptor)
    tpe == ObjType.Entity || tpe == ObjType.Enum
  }

  def decodeObjType(objTypeDef: Option[String]): ObjType = objTypeDef match {
    case Some("~") | Some("(inner)") => ObjType.Inner
    case Some("=") | Some("(enum)") => ObjType.Enum
    case Some("(entity)") => ObjType.Entity
    case Some(str) if str.startsWith("(base ") => ObjType.Base
    case None => ObjType.Entity
    case Some(_) => ObjType.Sub
  }

  def getFieldType(f: Ast.Leaf): FieldType = f.descriptor match {
    case Some("byte") => ByteField
    case Some("short") => ShortField
    case Some("int") => IntField
    case Some("boolean") => BooleanField
    case Some("date") => DateField
    case Some("datetime") => DatetimeField
    case Some("string") => StringField
    case Some(length) if length.forall(_.isDigit) => StringField
    case Some(_) => RefField
    case None => guessFieldType(f.name)
  }

  def guessFieldType(fieldName: String): FieldType = fieldName match {
    case "Date" | "Dob" | "From" | "Till" => DateField
    case "Datetime" | "Ctime" | "Mtime" => DatetimeField
    case "Price" | "Cost" | "Amount" | "Total" => MoneyField
    case "Count" => IntField
    case _ => StringField
  }

  def createFieldOrInner(leaf: Ast.Leaf, context: Context): Nested = {
    val fieldType = getFieldType(leaf)
    val size = leaf.descriptor match {
      case Some(num: String) if num.toList.forall(_.isDigit) => num.toInt
      case None if fieldType == StringField => 255
      case _ => 0
    }
    val ref: Option[String] = leaf.descriptor.map(value => if (value == "_") leaf.name else value)
    val inner: Option[InnerField] = leaf.descriptor.filter(context.isInner).flatMap(context.createInner(_, leaf.name))
    def field = ref match {
      case Some(refName) =>
        val refObj: Option[Obj] = context.createObj(refName, refName)
        val refFieldType = refObj match {
          case Some(_: Enum) => EnumField
          case _ => fieldType
        }
        Field(leaf.name, refFieldType, 0, refObj)
      case None => 
        Field(leaf.name, fieldType, size, None)
    }
    inner.getOrElse(field)
  }

  def createInner(node: Ast.Nesting, context: Context): InnerField = {
    val innerName = node.descriptor.filter(_ != "_").getOrElse(node.name)
    InnerField(node.name, Inner(innerName, module, getNested(node, context)))
  }

  def createSub(node: Ast.NestedNode, context: Context): SubField =  {
    val subName = node.descriptor.filter(_ != "_").getOrElse(node.name)
    SubField(node. name, Sub(subName, module, getNested(node, context)))
  }

  class Context(val types: Map[String, ObjType], val nodeMap: Map[String, Ast.Nesting], objCreator: (String, Ast.Nesting, Context) => Obj) {
    val cache: collection.mutable.Map[String, Obj] = collection.mutable.Map[String, Obj]()

    def containsObj(objName: String): Boolean = nodeMap.contains(objName)
    def createObj(objName: String, name: String): Option[Obj] = nodeMap.get(objName).map(topNode => objCreator(name, topNode, this))
    def createInner(objName: String, localName: String): Option[InnerField] = createObj(objName, objName) match {
      case Some(inner: Inner) => Some(InnerField(localName, inner))
      case _ => None
    }
    def isInner(objName: String): Boolean = types.get(objName).contains(ObjType.Inner)
  }

  def createObj(name: String, node: Ast.Nesting, context: Context): Obj = {
    decodeObjType(node.descriptor) match {
      case ObjType.Entity => Entity(name, module, getNested(node, context))
      case ObjType.Enum => Enum(name, module, node.nested.map(createEnumItem))
      case ObjType.Inner => Inner(name, module, getNested(node, context))
      case ObjType.Sub => Sub(node.nestingName, module, getNested(node, context))
    }
  }

  private def getNested(node: Ast.Nesting, context: Context) = {
    node.nested.map(n => createNested(n, context))
  }

  def createNested(node: Ast.Node, context: Context): Nested = node match {
    case f: Ast.Leaf => createFieldOrInner(f, context)
    case i: Ast.NestedNode if i.descriptor.isEmpty => createInner(i, context)
    case s: Ast.NestedNode => createSub(s, context)
  }
  
  def createEnumItem(node: Ast.Node): EnumItem = node match {
    case i: Ast.Leaf => EnumItem(i.name)
    case _ => throw new IllegalArgumentException("Enum item should be a leaf node")
  }
}