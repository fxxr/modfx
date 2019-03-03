package modfx

/**
  * Intermediate model
  */

object model {
  sealed trait FieldType {
    override def toString: String = this match {
      case StringField => "String"
      case IntField => "Int"
      case ByteField => "Byte"
      case ShortField => "Short"
      case DateField => "Date"
      case DateTimeField => "DateTime"
      case BooleanField => "Boolean"
      case MoneyField => "Money"
      case RefField => "Ref"
      case EnumField => "Enum"
    }
  }
  case object StringField extends FieldType
  case object IntField extends FieldType
  case object ByteField extends FieldType
  case object ShortField extends FieldType
  case object DateField extends FieldType
  case object DateTimeField extends FieldType
  case object BooleanField extends FieldType
  case object MoneyField extends FieldType
  case object RefField extends FieldType
  case object EnumField extends FieldType

  sealed trait NestingLevel
  case object GeneralNesting extends NestingLevel
  case object BasicNesting extends NestingLevel
  case object PlainNesting extends NestingLevel

  trait Nested {
    def name: String
    def toString(level: Int): String
    def fieldName: String = name.substring(0, 1).toLowerCase + name.substring(1)
  }

  case class Field(name: String, tpe: FieldType, size: Int = 0, ref: Option[Obj]) extends Nested {
    override def toString(level: Int): String = List(
      s"$name [$tpe]",
      if (size > 0 && !(size == 255 && tpe == StringField)) size.toString else "",
      ref.map("(" + _.name + ")").getOrElse("")
    ).filter(_.nonEmpty).mkString(" ")

    def refName: String = ref.map(_.name).getOrElse("???")

    def refModuleName: String = ref.map(_.module).getOrElse("")
  }

  case class RefField(name: String, ref: Obj)


  sealed trait Obj extends Nested {
    def module: String
    def nested: List[Nested]
    def fields: List[Field] = nested.flatMap{
      case f: Field => f :: Nil;
      case _ => List.empty
    }
    def nestingLevel: NestingLevel

    override def toString: String = {
      toString(0)
    }

    def toString(level: Int): String = {
      val objTypeName = getClass.getSimpleName.toLowerCase
      s"$name [$objTypeName]\n" + toStringNested(nested, level + 1)
    }

    private def toStringNested(nested: List[Nested], level: Int) = nested.map("  " * level + _.toString(level + 1)).mkString("\n")
  }

  trait BasicObj extends Obj {
    def inners: List[InnerField] = nested.flatMap{
      case i: InnerField => List(i)
      case _ => List.empty
    }
    override def nestingLevel: NestingLevel = if (inners.nonEmpty) BasicNesting else PlainNesting
  }

  trait WithSubs extends BasicObj {
    def subs: List[SubField] = nested.flatMap{
      case s: SubField => List(s)
      case _ => List.empty
    }
    override def nestingLevel: NestingLevel = subs match {
      case Nil if inners.isEmpty => PlainNesting
      case Nil => BasicNesting
      case _ => GeneralNesting
    }
  }

  case class Entity(name: String, module: String, nested: List[Nested]) extends WithSubs
  
  case class EntityView(name: String, module: String, base: String, nested: List[Nested]) extends BasicObj

  case class SubField(name: String, sub: Sub) extends Nested {
    override def toString(level: Int): String = name + " => " + sub.toString(level)
  }

  case class InnerField(name: String, inner: Inner) extends Nested {
    override def toString(level: Int): String = name + " => " + inner.toString(level)
  }

  case class Sub(name: String, module: String, nested: List[Nested]) extends WithSubs

  case class Inner(name: String, module: String, nested: List[Nested], container: Option[String]) extends BasicObj

  case class Base(name: String, module: String, nested: List[Nested]) extends BasicObj

  case class Enum(name: String, module: String, nested: List[Nested]) extends Obj {
    override def nestingLevel: NestingLevel = PlainNesting
  }
  
  case class EnumItem(name: String) extends Nested {
    override def toString(level: Int): String = name
  }
}