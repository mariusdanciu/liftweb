package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class XmlExamplesTest extends Runner(XmlExamples) with JUnit
object XmlExamples extends Specification {
  import JsonAST._
  import JsonDSL._
  import Xml._
  import scala.xml.{Group, Text}

  "Basic conversion example" in {
    val json = toJson(users1) 
    compact(render(json)) mustEqual """{"users":{"count":"2","user":[{"disabled":"true","id":"1","name":"Harry"},{"id":"2","name":"David","nickname":"Dave"}]}}"""
  }

  "Conversion transformation example 1" in {
    val json = toJson(users1) map {
      case JField("id", JString(s)) => JField("id", JInt(s.toInt))
      case x => x 
    }
    compact(render(json)) mustEqual """{"users":{"count":"2","user":[{"disabled":"true","id":1,"name":"Harry"},{"id":2,"name":"David","nickname":"Dave"}]}}"""
  }

  "Conversion transformation example 2" in {
    val json = toJson(users2) map {
      case JField("id", JString(s)) => JField("id", JInt(s.toInt))
      case JField("user", x: JObject) => JField("user", JArray(x :: Nil))
      case x => x 
    }
    compact(render(json)) mustEqual """{"users":{"user":[{"id":1,"name":"Harry"}]}}"""
  }

  "Primitive array example" in {
    val xml = <chars><char>a</char><char>b</char><char>c</char></chars>
    compact(render(toJson(xml))) mustEqual """{"chars":{"char":["a","b","c"]}}"""
  }

  "Lotto example which flattens number arrays into encoded string arrays" in {
    def flattenArray(nums: List[JValue]) = JString(nums.map(_.values).mkString(","))

    val printer = new scala.xml.PrettyPrinter(100,2)
    val lotto: JObject = LottoExample.json
    val xml = toXml(lotto map {
      case JField("winning-numbers", JArray(nums)) => JField("winning-numbers", flattenArray(nums))
      case JField("numbers", JArray(nums)) => JField("numbers", flattenArray(nums))
      case x => x
    })

    printer.format(xml(0)) mustEqual printer.format(
      <lotto>
        <id>5</id>
        <winning-numbers>2,45,34,23,7,5,3</winning-numbers>
        <winners>
          <winner-id>23</winner-id>
          <numbers>2,45,34,23,3,5</numbers>
        </winners>
        <winners>
          <winner-id>54</winner-id>
          <numbers>52,3,12,11,18,22</numbers>
        </winners>
      </lotto>)
  }

  "Band example with namespaces and empty tag" in {
    val json = toJson(band)
    Printer.pretty(render(json)) mustEqual """{
  "b:band":{
    "name":"The Fall",
    "genre":"rock",
    "influence":null,
    "playlists":{
      "playlist":[{
        "name":"empty"
      },{
        "name":"mid 80s",
        "song":["Eat your self fitter","My new house"]
      }]
    }
  }
}"""
  }

  "Grouped text example" in {
    val json = toJson(groupedText)
    compact(render(json)) mustEqual """{"g":{"group":"foobar","url":"http://example.com/test"}}"""
  }

  val users1 =
    <users count="2">
      <user disabled="true">
        <id>1</id>
        <name>Harry</name>
      </user>
      <user>
        <id>2</id>
        <name nickname="Dave">David</name>
      </user>
    </users>   

  val users2 =
    <users>
      <user>
        <id>1</id>
        <name>Harry</name>
      </user>
    </users>

  val band =
    <b:band>
      <name>The Fall</name>
      <genre>rock</genre>
      <influence/>
      <playlists>
        <playlist name="empty" />
        <playlist name="mid 80s">
          <song>Eat your self fitter</song>
          <song>My new house</song>
        </playlist>
      </playlists>
    </b:band>

  val url = "test"
  val groupedText =
    <g>
      <group>{ Group(List(Text("foo"), Text("bar"))) }</group>
      <url>http://example.com/{ url }</url>
    </g>
}
