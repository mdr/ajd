package com.github.mdr.ajd

import com.fasterxml.jackson.annotation.JsonProperty
import org.scalatest.{ FlatSpec, Matchers }
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class Diagram(name: String, circles: Seq[Circle])

case class Circle(centre: Point, radius: Int)

case class Point(x: Int, y: Int, z: Option[Int] = None)

case class Customer(@JsonProperty("customer_id") customerId: String)

class BasicTest extends FlatSpec with Matchers {

  "Deserializing JSON" should "work" in {
    val json = parse(
      """
        |{
        |  "name": "Best Diagram",
        |  "circles": [
        |    { "centre": { "x": 1, "y": 2 }, "radius": 100 },
        |    { "centre": { "x": 3, "y": 4, "z": 5 }, "radius": 50 }
        |  ]
        |}""".stripMargin)

    val actualDiagram = new JsonDeserializer().deserialize[Diagram](json)

    val expectedDiagram =
      Diagram(
        "Best Diagram",
        circles = Seq(
          Circle(Point(x = 1, y = 2), radius = 100),
          Circle(Point(x = 3, y = 4, z = Some(5)), radius = 50)))

    actualDiagram shouldEqual expectedDiagram
  }

  it should "remap property names" in {

    val json = parse(
      """
        |{
        |  "customer_id": "001"
        |}
      """.stripMargin)

    val actualCustomer = new JsonDeserializer().deserialize[Customer](json)

    val expectedCustomer = Customer("001")

    actualCustomer shouldEqual expectedCustomer
  }

}
