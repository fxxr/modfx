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
      case DatetimeField => "Datetime"
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
  case object DatetimeField extends FieldType
  case object BooleanField extends FieldType
  case object MoneyField extends FieldType
  case object RefField extends FieldType
  case object EnumField extends FieldType

  sealed trait EntityType
  case object GeneralEntity extends EntityType
  case object PlainEntity extends EntityType
  case object SimpleEntity extends EntityType

  trait Nested {
    def name: String
    def toString(level: Int): String
    def fieldName: String = name.substring(0, 1).toLowerCase + name.substring(1)
  }
  
  case class Field(name: String, tpe: FieldType, size: Int = 0, ref: Option[Obj]) extends Nested {
    override def toString(level: Int): String = Seq(
      s"$name [$tpe]",
      if (size > 0 && !(size == 255 && tpe == StringField)) size.toString else "",
      ref.map("(" + _.name + ")").getOrElse("")
    ).filter(_.nonEmpty).mkString(" ")
    
    def refName: String = ref.map(_.name).getOrElse("???")
  }

  sealed trait Obj extends Nested {
    def module: String
    def nested: Seq[Nested]
    def fields: Seq[Field] = nested.flatMap{
      case f: Field => f :: Nil;
      case _ => Seq.empty
    }

    override def toString: String = {
      toString(0)
    }

    def toString(level: Int): String = {
      val objTypeName = getClass.getSimpleName.toLowerCase
      s"$name [$objTypeName]\n" + toStringNested(nested, level + 1)
    }

    private def toStringNested(nested: Seq[Nested], level: Int) = nested.map("  " * level + _.toString(level + 1)).mkString("\n")
  }

  trait PlainObj extends Obj {
    def inners: Seq[InnerField] = nested.flatMap{
      case i: InnerField => Seq(i)
      case _ => Seq.empty
    }
  }

  trait WithSubs extends PlainObj {
    def subs: Seq[SubField] = nested.flatMap{
      case s: SubField => Seq(s)
      case _ => Seq.empty
    }
  }

  case class Entity(name: String, module: String, nested: Seq[Nested]) extends WithSubs {
    def entityType: EntityType = subs match {
      case Nil if inners.isEmpty => SimpleEntity
      case Nil => PlainEntity
      case _ => GeneralEntity
    }
    val meta: String = name + "Meta"
    val metaInstance: String = metaInstName(name)
    val fieldMeta: String = meta + ".Field"
    val innerMeta: String = meta + ".Inner"
    val subMeta: String = meta + ".Sub"
  }
  
  case class SubField(name: String, sub: Sub) extends Nested {
    override def toString(level: Int): String = name + " => " + sub.toString(level)
  }

  case class InnerField(name: String, inner: Inner) extends Nested {
    override def toString(level: Int): String = name + " => " + inner.toString(level)
  }

  case class Sub(name: String, module: String, nested: Seq[Nested]) extends WithSubs

  case class Inner(name: String, module: String, nested: Seq[Nested]) extends PlainObj

  case class Base(name: String, module: String, nested: Seq[Nested]) extends PlainObj

  case class Enum(name: String, module: String, nested: Seq[Nested]) extends Obj
  
  case class EnumItem(name: String) extends Nested {
    override def toString(level: Int): String = name
  }
    
  def splitCamelCase(str: String): Seq[String] = str.split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])")

  def metaInstName(name: String): String = splitCamelCase(name).map(_.toUpperCase).mkString("_")
}