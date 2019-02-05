package modfx

import scala.reflect.ClassTag

/**
  * Java AST
  */

package object javamodel {
  case class JavaFile(name: SimpleName, tpe: FileType, pack: PackageDecl, imports: Seq[ImportDecl], mainType: TypeDecl) extends Node
  case class PackageDecl(name: Name) extends Declaration
  case class ImportDecl(path: Name, tpe: ImportType) extends Statement

  trait Node
  trait Statement extends Node
  trait Declaration extends Statement
  trait Expression extends Statement
  
  sealed trait FileType
  final case object ModelFile extends FileType
  final case object MetaFile extends FileType

  type Name = String
  type SimpleName = String

  sealed class TypeDecl(name: SimpleName, modifiers: Seq[Modifier], extended: Option[Type], body: Block) extends Declaration
  case class ClassDecl(name: SimpleName, modifiers: Seq[Modifier], extended: Option[Type], implemented: Seq[Type], body: Body)
    extends TypeDecl(name, modifiers, extended, body)
  case class InterfaceDecl(name: SimpleName, modifiers: Seq[Modifier], extended: Option[Type], body: Body)
    extends TypeDecl(name, modifiers, extended, body)
  case class EnumDecl(name: SimpleName, implemented: Seq[Type], body: EnumBody)
    extends TypeDecl(name, Seq(Public), None, body)
  case class EnumEntry(name: SimpleName) extends Node

  case class Type(name: Name, typeParams: TypeParams = TypeParams.empty, isArray: Boolean = false) extends Node
  case class TypeParams(params: String*) extends Node
  object TypeParams {
    val empty = TypeParams()
  }
  case class Param(tpe: Type, name: SimpleName) extends Node
  type Params = Seq[Param]
  type CallParams = Seq[Expression]
  
  case class Body(declarations: Seq[Declaration]) extends Block(declarations)
  case class EnumBody(entries: Seq[EnumEntry], enumDeclarations: Seq[Declaration]) extends Block(enumDeclarations)

  sealed class Block(val statements: Seq[Statement]) extends Expression

  case class ConstructorDecl(name: SimpleName, params: Params, statements: Seq[Statement]) extends Declaration
  case class FieldDecl(tpe: Type, name: SimpleName, modifiers: Seq[Modifier]) extends Declaration
  case class MethodDecl(tpe: Type, name: SimpleName, modifiers: Seq[Modifier], overriding: Boolean, params: Params, body: Block) extends Declaration
  case class FieldAssign(context: Name, name: Name, expr: Expression) extends Expression
  case class MethodCall(context: Name = "", name: Name, params: CallParams) extends Expression
  case class AssignExpr(variable: Name, value: Expression) extends Expression
  case class Return(value: Option[Expression]) extends Statement
  case class Super(params: CallParams) extends Statement
  case class StatementExpr(expression: Expression) extends Statement
  case class NameExpr(name: Name) extends Expression
  case class Switch(expr: Expression, cases: Seq[Case]) extends Statement
  case class Case(name: Name, statements: Seq[Statement]) extends Statement
  case class Func(params: Params, body: Expression) extends Expression
  case class ConstructorCall(name: SimpleName, params: CallParams) extends Expression

  sealed trait ImportType
  case object ProjectImport extends ImportType
  case object StdImport extends ImportType
  case object StaticImport extends ImportType

  sealed trait Modifier
  sealed trait AccessModifier extends Modifier
  case object Public extends AccessModifier
  case object Private extends AccessModifier
  case object Static extends Modifier
  case object Final extends Modifier

  object Block {
    def apply[X: ClassTag](statements: Statement*) = new Block(statements.toSeq)
  }
  object Body {
    def apply[X: ClassTag](declarations: Declaration*) = new Body(declarations.toSeq)
  }
  object Return {
    def apply(value: Expression) = new Return(Some(value))
    def apply(value: String) = new Return(Some(NameExpr(value)))
  }
  object Case {
    def apply(name: Name, statement: Statement) = new Case(name, Seq(statement))
  }
  object MethodCall {
    def apply(name: Name, param: Expression) = new MethodCall("", name, Seq(param))
    def apply(name: Name) = new MethodCall("", name, Seq.empty)
  }
  object Super {
    def apply[X: ClassTag](params: Expression*) = new Super(params.toSeq)
  }
  implicit def strToNameExpr(str: String): NameExpr = NameExpr(str)
}
