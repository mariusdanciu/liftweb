package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import scala.reflect.Manifest
import JsonAST._
import JsonParser.parse

/** Functions to serialize and deserialize a case class.
 *
 *  FIXME: add Map support
 * 
 *  See: SerializationExamples.scala
 */
object Serialization {
  import java.io.{StringWriter, Writer}
  import Meta.Reflection._

  val formats = new Formats {
    val dateFormat = DefaultFormats.lossless.dateFormat
    val typeInformation = Always
  }

  def write[A <: AnyRef](a: A): String = write(a, new StringWriter).toString

  def write[A <: AnyRef, W <: Writer](a: A, out: W): W = 
    Printer.compact(render(Extraction.decompose(a)(formats)), out)

  def read[A](json: String)(implicit mf: Manifest[A]): A = parse(json).extract(formats, mf)
}
