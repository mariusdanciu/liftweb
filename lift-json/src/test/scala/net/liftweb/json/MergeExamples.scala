package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class MergeExamplesTest extends Runner(MergeExamples) with JUnit
object MergeExamples extends Specification {
  import JsonAST._
  import JsonParser._

  val scala1 = parse("""
    {
      "lang": "scala",
      "tags": ["fp", "oo"],
      "features": [
        { "key1":"val1" },
        { "key2":"val2" }
      ]
    }""")

  val scala2 = parse("""
    {
      "tags": ["static-typing","fp"],
      "compiled": true,
      "lang": "scala",
      "features": [
        { "key2":"newval2" }
        { "key3":"val3" }
      ]
    }""")

  val expectedMergeResult = parse("""
    {
    }""")
}
