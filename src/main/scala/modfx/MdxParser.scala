package modfx

/**
  * Mdx file parser
  */
object MdxParser {
  import fastparse.all._
  import model._

  object Ast {
    sealed trait Node {
      def name: String
      def descriptor: Option[String]
    }
    trait Nesting extends Node {
      def nested: List[Node]
      def nesting: List[Nesting] = nested.flatMap{
        case n: Nesting => n :: Nil
        case _ => Nil
      }
      def nestingName: String = this match {
        case TopNode(name, _, _) => name
        case NestedNode(name, descriptor, _) => descriptor.filterNot(_ == "_").getOrElse(name)
      }
    }
    case class TopNode(name: String, descriptor: Option[String], nested: List[Node]) extends Nesting
    case class NestedNode(name: String, descriptor: Option[String], nested: List[Node]) extends Nesting
    case class Leaf(name: String, descriptor: Option[String]) extends Node
  }


  object BaseParsers {
    val lowercase = P( CharIn('a' to 'z') )
    val uppercase = P( CharIn('A' to 'Z') )
    val letter = P( lowercase | uppercase )
    val digit = P( CharIn('0' to '9') )
    val intNumber = P( digit.rep(1) )
    val newline = P( "\n" )
    val whitespace = P( newline | " " )

    val name = P( uppercase ~ letter.rep(1) )
    val fieldName = P( "+".? ~ name )
    val moduleName = P( letter.rep(1) )
    val path = P( (moduleName ~ "/").? ~ name ~ ("." ~ name).rep(0) )
    val fieldTypes = List("string", "date", "byte", "short", "datetime", "money", "int", "boolean", "money")
    val objTypes = List("entity", "enum", "inner")
    val spacesPerLevel = 2

    def indent(level: Int) = P( " " * spacesPerLevel * level )
    def nodeSep(level: Int) = P( newline ~ indent(level) )
  }

  import Ast._

  def parseToAst(input: String): Either[String, Seq[Ast.TopNode]] = {
    import BaseParsers._

    val objDescriptor = P( CharIn("~=:").! | ("(" ~ (StringIn(objTypes:_*) | name).! ~ ")") )
    val nestedDescriptor = P( (intNumber | StringIn(fieldTypes:_*) | path | "_").rep(0, "," ~ " ".?) )

    val leaf: Parser[Node] = P( fieldName.! ~ (" ".rep(1) ~ "(" ~ nestedDescriptor.! ~ ")").? )
      .map{ case (nme, dsc) => Leaf(nme, dsc) }
    val nestedNode: Parser[Node] = P( name.! ~ (" ".rep(1) ~ "(" ~ nestedDescriptor.! ~ ")").? )
      .map{ case (nme, dsc) => NestedNode(nme, dsc, List.empty) }
    def nestedBranch(level: Int): Parser[Node] = P( nestedNode ~ nestedList(level + 1) )
      .map{ case (nn, subNested) => NestedNode(nn.name, nn.descriptor, subNested.toList) }

    def nested(level: Int) = P( nestedBranch(level) | leaf )
    def nestedList(level: Int): Parser[Seq[Node]] = P( nodeSep(level) ~ nested(level).rep(1, sep = nodeSep(level)) )
    val objHeader = P(name.! ~ (" ".rep(1) ~ objDescriptor).?)
    val obj = P( objHeader ~ nestedList(1) ).map{ case (nme, tpe, nst) => TopNode(nme, tpe, nst.toList) }
    val objs = P( obj.rep(1, sep = newline.rep(1)) )
    val mdx: Parser[Seq[TopNode]] = P( Start ~ whitespace.rep(0) ~ objs ~ whitespace.rep(0) ~ End)

    val trimmedInput = input.split('\n').map(_.replaceAll("\\s+$", "")).mkString("\n")
    mdx.parse(trimmedInput) match {
      case Parsed.Success(res, _) => Right(res)
      case Parsed.Failure(_,_,extra) => Left(extra.traced.trace)
    }
  }

  def parse(topNodes: Seq[Ast.TopNode], module: String): Seq[Obj] = {
    new ModelCreator(topNodes, module).create()
  }

  def parse(input: String, module: String): Either[String, Seq[Obj]] = {
    parseToAst(input).map(nodes => parse(nodes, module))
  }
}
