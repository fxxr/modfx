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
    case object EntityView extends ObjType
    case object Enum extends ObjType
  }

  def create(): Seq[Obj] = {
    val objTypes: Map[String, ObjType] = topNodes.map(n => n.name -> decodeObjType(n.descriptor)).toMap
    def collectNodes(node: Ast.Nesting): Seq[(String, Ast.Nesting)] = 
      (node.nestingName -> node) +: node.nesting.filter(_.descriptor.isDefined).flatMap(collectNodes)
    val nodeMap: Map[String, Ast.Nesting] = topNodes.flatMap(collectNodes).toMap
    topNodes.map(n => createObj(n.name, n, new Context(objTypes, nodeMap, createObj)))
  }

  def decodeObjType(objTypeDef: Option[String]): ObjType = objTypeDef match {
    case Some("~") => ObjType.Inner
    case Some("=") => ObjType.Enum
    case Some(_) => ObjType.EntityView
    case None => ObjType.Entity
  }

  def getFieldType(f: Ast.Leaf): FieldType = f.descriptor match {
    case Some("byte") => ByteField
    case Some("short") => ShortField
    case Some("int") => IntField
    case Some("boolean") => BooleanField
    case Some("date") => DateField
    case Some("datetime") => DateTimeField
    case Some("string") => StringField
    case Some("money") => MoneyField
    case Some(length) if length.forall(_.isDigit) => StringField
    case Some(_) => RefField
    case None => guessFieldType(f.name)
  }

  def guessFieldType(fieldName: String): FieldType = fieldName match {
    case "Date" | "Dob" | "From" | "Till" => DateField
    case "Datetime" | "Ctime" | "Mtime" => DateTimeField
    case "Price" | "Cost" | "Amount" | "Total" => MoneyField
    case "Count" => IntField
    case _ => StringField
  }
  
  sealed trait DescriptorContent
  case object EmptyDesc extends DescriptorContent
  case object SizeDesc extends DescriptorContent
  case object TypeDesc extends DescriptorContent
  case class RefDesc(ref: Obj) extends DescriptorContent
  case class InnerDesc(inner: InnerField) extends DescriptorContent
  case class UnknownDesc(unresolved: String) extends DescriptorContent
  
  private def handleDescriptorContent(desc: Option[String], fieldName: String, context: Context): DescriptorContent = 
    desc.map(d => if (d == "_") fieldName else d) match {
    case None => EmptyDesc
    case Some(num) if isInt(num) => SizeDesc
    case Some(typeName) if MdxParser.BaseParsers.fieldTypes.contains(typeName) => TypeDesc
    case Some(inner) if context.isInner(inner) => context.createInner(inner, fieldName)
      .map(InnerDesc).getOrElse(UnknownDesc(s"$fieldName ($inner)"))
    case Some(refName) =>
      resolveRef(refName, context).map(RefDesc).getOrElse(UnknownDesc(s"$fieldName ($refName)"))
    case Some(dsc) => UnknownDesc(s"$fieldName ($dsc)")
  }

  private def size(desc: Option[String]): Int = desc match {
    case Some(num: String) if isInt(num) => num.toInt
    case _ => 0
  }

  def createFieldOrInner(leaf: Ast.Leaf, context: Context): Nested = {
    val fieldType = getFieldType(leaf)
    handleDescriptorContent(leaf.descriptor, leaf.name, context) match {
      case SizeDesc | EmptyDesc | TypeDesc =>
        Field(leaf.name, fieldType, size(leaf.descriptor), None)
      case InnerDesc(innerField) => innerField
      case RefDesc(ref) =>
        val refFieldType = ref match {
          case _: Enum => EnumField
          case _ => RefField
        }
        Field(leaf.name, refFieldType, 0, Some(ref))
      case UnknownDesc(unresolved) => Field(s"UNRESOLVED $unresolved", RefField, 0, None)
    }
  }
  
  private def isInt(str: String): Boolean = str forall Character.isDigit
  
  private def resolveRef(refDesc: String, context: Context): Option[Obj] = refDesc.split('/').toList match {
    case mod :: obj :: Nil => Some(Entity(obj, mod, List.empty))
    case obj :: Nil => context.createObj(obj, obj)
    case _ => None
  }

  def createInner(node: Ast.Nesting, parent: Ast.Node, context: Context): InnerField = {
    val innerName = node.descriptor.filter(_ != "_").getOrElse(node.name)
    InnerField(node.name, Inner(innerName, module, getNested(node, context), container = Some(parent.name)))
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
      case ObjType.Inner => Inner(name, module, getNested(node, context), None)
      case ObjType.Sub => Sub(node.nestingName, module, getNested(node, context))
      case ObjType.EntityView => EntityView(name, module, node.descriptor.getOrElse("??"), getNested(node, context))
    }
  }

  private def getNested(node: Ast.Nesting, context: Context) = {
    node.nested.map(n => createNested(n, node, context))
  }

  def createNested(node: Ast.Node, parent: Ast.Node, context: Context): Nested = node match {
    case f: Ast.Leaf => createFieldOrInner(f, context)
    case i: Ast.NestedNode if i.descriptor.isEmpty => createInner(i, parent, context)
    case s: Ast.NestedNode => createSub(s, context)
  }
  
  def createEnumItem(node: Ast.Node): EnumItem = node match {
    case i: Ast.Leaf => EnumItem(i.name)
    case _ => throw new IllegalArgumentException("Enum item should be a leaf node")
  }
}