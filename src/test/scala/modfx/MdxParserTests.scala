package modfx

import utest._

object MdxParserTests extends TestSuite {
  val tests = Tests {
    "Flat objects" - {
      val Right(objs) = MdxParser.parseToAst(
        """User
          |  Name
          |  Role     (_)
          |  Created  (datetime)
          |  Rank     (int)
          |
          |Role =
          |  Administrator
          |  Manager
          |  Director
        """.stripMargin)

      assert(objs.length == 2)
      assert(objs(0).toString == "TopNode(User,None,ArrayBuffer(Leaf(Name,None), Leaf(Role,Some(_)), Leaf(Created,Some(datetime)), Leaf(Rank,Some(int))))")
      assert(objs(1).toString == "TopNode(Role,Some(=),ArrayBuffer(Leaf(Administrator,None), Leaf(Manager,None), Leaf(Director,None)))")
    }

    "Nested objects" - {
      val Right(objs) = MdxParser.parseToAst(
        """Order
          |  Number
          |  Date
          |  Customer  (_)
          |  Items   (OrderItem)
          |    Num       (short)
          |    Product   (_)
          |    Quantity  (short)
          |    Cost      (money)
          |
          |
          |ProductBase (base Product)
          |  Code   (10)
          |  Title
          |  Price  (money)
        """.stripMargin)
      assert(objs.length == 2)
      assert(objs.head.toString == "TopNode(Order,None,ArrayBuffer(Leaf(Number,None), Leaf(Date,None), Leaf(Customer,Some(_)), NestedNode(Items,Some(OrderItem),ArrayBuffer(Leaf(Num,Some(short)), Leaf(Product,Some(_)), Leaf(Quantity,Some(short)), Leaf(Cost,Some(money))))))")
      assert(objs.tail.head.toString == "TopNode(ProductBase,Some((base Product)),ArrayBuffer(Leaf(Code,Some(10)), Leaf(Title,None), Leaf(Price,Some(money))))")
    }
  }
}
