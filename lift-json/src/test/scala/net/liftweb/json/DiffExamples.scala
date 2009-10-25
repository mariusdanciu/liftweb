package net.liftweb.json

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class DiffExamplesTest extends Runner(DiffExamples) with JUnit
object DiffExamples extends Specification {
  import JsonAST._
  import JsonParser._
  import MergeExamples.{scala1, scala2}

  "Diff example" in {
    val Diff(changed, added, deleted) = scala1 diff scala2
    changed mustEqual expectedChanges
    added mustEqual expectedAdditions
    deleted mustEqual expectedDeletions
  }

  val expectedChanges = parse("""
    { 
      "tags": ["static-typing","fp"],
      "features": { 
        "key2":"newval2" 
      } 
    }""")

  val expectedAdditions = parse("""
    { 
      "features": { 
        "key3":"val3" 
      },
      "compiled": true 
    }""")

  val expectedDeletions = parse("""
    { 
      "year":2006, 
      "features":{ "key1":"val1" } 
    }""")
}
