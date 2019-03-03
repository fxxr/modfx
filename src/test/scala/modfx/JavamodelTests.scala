package modfx

import modfx.javamodel._
import utest._

/**
  * User: Anatoly
  * Date: 02.09.2018
  * Time: 16:35
  */
object JavamodelTests extends TestSuite {
  
  val tests = Tests {
    "Simple class" - {
      val license = JavaFile(
        "License",
        ModelFile,
        PackageDecl("licensing.model"), 
        List(
          ImportDecl("common.base.Guid", ProjectImport),
          ImportDecl("common.base.model.Entity", ProjectImport),
          ImportDecl("licensing.model.meta.LicenseMeta.LICENSE", StaticImport)
        ),
        ClassDecl(
          "License", List(Public), 
          Some(Type("Entity.Plain", TypeParams("LicenseMeta", "LicenseMeta.Field", "LicenseMeta.Inner"))),
          List.empty,
          Body(
            FieldDecl(Type("LicenseState"), "state", List(Public, Final)),
            FieldDecl(Type("LicenseDetails"), "license", List(Public, Final)),
            ConstructorDecl("License", params = List(
                Param(Type("int"), "id"),
                Param(Type("Guid"), "guid"),
                Param(Type("LicenseState"), "state"),
                Param(Type("LicenseDetails"), "license")
              ), statements = List(
                Super(NameExpr("id"), NameExpr("guid")),
                FieldAssign("this", "state", NameExpr("state")),
                FieldAssign("this", "license", NameExpr("license"))
              )
            ),
            MethodDecl(Type("LicenseMeta"), "meta", List(Public), overriding = true, List.empty, Block(
              Return(NameExpr("LICENSE"))
            )),
            MethodDecl(Type("Object"), "getValue", List(Public), overriding = true, List(Param(Type("LicenseMeta.Field"), "field")), Block(
              Switch(NameExpr("field"), List(
                Case("State", Return("state")),
                Case("License", Return("license"))
              )),
              Return(MethodCall("throwNotImplemented", NameExpr("field"))))
            ))
          )
        )
      assert(JavaModelFormatter.toString(license) == 
        """package licensing.model;
          |
          |import common.base.Guid;
          |import common.base.model.Entity;
          |import static licensing.model.meta.LicenseMeta.LICENSE;
          |
          |public class License extends Entity.Plain<LicenseMeta, LicenseMeta.Field, LicenseMeta.Inner> {
          |    public final LicenseState state;
          |    public final LicenseDetails license;
          |    
          |    public License(int id, Guid guid, LicenseState state, LicenseDetails license) {
          |        super(id, guid);
          |        this.state = state;
          |        this.license = license;
          |    }
          |    
          |    @Override
          |    public LicenseMeta meta() {
          |        return LICENSE;
          |    }
          |    
          |    @Override
          |    public Object getValue(LicenseMeta.Field field) {
          |        switch (field) {
          |            case State: return state;
          |            case License: return license;
          |        }
          |        return throwNotImplemented(field);
          |    }
          |}""".stripMargin
       )
    }

    "Simple enum" - {
      val licenseState = JavaFile(
        "LicenseState",
        ModelFile,
        PackageDecl("licensing.model"),
        List.empty,
        EnumDecl("LicenseState", List.empty, EnumBody(List(
          EnumEntry("Active"),
          EnumEntry("Expired"),
          EnumEntry("Revoked")
        ), List.empty))
      )
      assert(JavaModelFormatter.toString(licenseState) ==
        """package licensing.model;
          |
          |
          |public enum LicenseState {
          |    Active,
          |    Expired,
          |    Revoked
          |}""".stripMargin
      )
    }
  }
}
