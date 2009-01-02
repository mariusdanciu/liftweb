/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
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

package net.liftweb.http

import _root_.scala.xml.Node
import _root_.net.liftweb.util._
import js._
import _root_.javax.servlet.http.Cookie

object JavaScriptResponse {
  def apply(js: JsCmd): LiftResponse = JavaScriptResponse(js, Nil, Nil, 200)
}

/**
 * Impersonates a HTTP response having Content-Type = text/javascript
 */
case class JavaScriptResponse(js: JsCmd, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = js.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/javascript") :: headers, cookies, code)
  }
}