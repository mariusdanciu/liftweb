package net.liftweb.json

import java.util.Date
import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class SerializationExamplesTest extends Runner(SerializationExamples) with JUnit
object SerializationExamples extends Specification {
  import Serialization.{read, write => swrite}

  implicit val formats = Serialization.formats(TypeHints((classOf[Animal], TypeHints.shortName)))

  val project = Project("test", new Date, Some(Language("Scala", 2.75)), List(
    Team("QA", List(Employee("John Doe", 5), Employee("Mike", 3))),
    Team("Impl", List(Employee("Mark", 4), Employee("Mary", 5), Employee("Nick Noob", 1)))))

  "Project serialization example" in {
    val ser = swrite(project)
    read[Project](ser) mustEqual project
  }

  case class Project(name: String, startDate: Date, lang: Option[Language], teams: List[Team])
  case class Language(name: String, version: Double)
  case class Team(role: String, members: List[Employee])
  case class Employee(name: String, experience: Int)

  "Null example" in {
    val ser = swrite(Nullable(null))
    read[Nullable](ser) mustEqual Nullable(null)
  }

  case class Nullable(name: String)
  
  "Lotto serialization example" in {
    import LottoExample.{Lotto, lotto}

    val ser = swrite(lotto)
    read[Lotto](ser) mustEqual lotto
  }

  "Primitive serialization example" in {
    val primitives = Primitives(124, 123L, 126.5, 127.5.floatValue, "128", 125, 129.byteValue, true)
    val ser = swrite(primitives)
    read[Primitives](ser) mustEqual primitives
  }

  "Polymorphic List serialization example" in {
//    val animals = Animals(Dog("pluto") :: Fish(1.2) :: Dog("devil") :: Nil)
//    val ser = swrite(animals)
//    read[Animals](ser) mustEqual animals
  }

  case class Animals(animals: List[Animal])
  trait Animal
  case class Dog(name: String) extends Animal
  case class Fish(weight: Double) extends Animal
}
