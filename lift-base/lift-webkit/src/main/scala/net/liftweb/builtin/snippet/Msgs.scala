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
package net.liftweb.builtin.snippet;

import _root_.net.liftweb.http._
import _root_.scala.xml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util.Log
import S._
import _root_.net.liftweb.common.{Box, Full, Empty}


/**
 * This built in snippet renders messages (Errors, Warnings, Notices) in a <i>div</i>.
 * Typically it is used in templates as a place holder for any messages that are <b>not</b> associated with an ID.
 * Setting the attribute <i>showAll</i> to <i>true</i> will render all messages, with and without an ID.
 * This will lead to duplicate messages if additionally the <i>Msg</i> built in snippet is used to show
 * messages associated with an ID.
 *
 * E.g. (child nodes are optional)
 * <pre>
 * &lt;lift:Msgs showAll="false"&gt;
 *   &lt;lift:error_msg&gt;Error!  The details are:&lt;/lift:error_msg&gt;
 *   &lt;lift:error_class&gt;errorBox&lt;/lift:error_class&gt;
 *   &lt;lift:warning_msg&gt;Whoops, I had a problem:&lt;/lift:warning_msg&gt;
 *   &lt;lift:warning_class&gt;warningBox&lt;/lift:warning_class&gt;
 *   &lt;lift:notice_msg&gt;Note:&lt;/lift:notice_msg&gt;
 *   &lt;lift:notice_class&gt;noticeBox&lt;/lift:notice_class&gt;
 * &lt;/lift:snippet&gt;
 * </pre>
 *
 */
object Msgs extends DispatchSnippet {
  def dispatch: DispatchIt = {
    case _ => render
  }

  def render(styles: NodeSeq): NodeSeq = {
    val f = if (toBoolean(attr("showAll"))) messages _
            else noIdMessages _

    val makeTitle: (String) => String = {text =>
      Log.debug("Msgs: Default " + text + " is not rendered as the default title is now empty string")
      ""
    }

    val msgs = List((f(S.errors),
                     (styles \\ "error_msg"), S.??("msg.error"),
                     ((styles \\ "error_class") ++
                      (styles \\ "error_msg" \\ "@class")), "error"),
                    (f(S.warnings),
                     (styles \\ "warning_msg"), S.??("msg.warning"),
                     ((styles \\ "warning_class")++
                      (styles \\ "warning_msg" \\ "@class")), "warn"),
                    (f(S.notices),
                     (styles \\ "notice_msg"), S.??("msg.notice"),
                     ((styles \\ "notice_class")) ++
                     (styles \\ "notice_msg" \\ "@class"), "notice")).flatMap
    {
      case (msg, titleList, defaultTitle, styleList, ord) =>
        val title: String = titleList.toList.filter(_.prefix == "lift").
        map(_.text.trim).filter(_.length > 0) headOr makeTitle(defaultTitle)
        val styles = styleList.toList.map(_.text.trim)
        if (!styles.isEmpty) {
          ord match {
            case "error" => MsgsErrorMeta(Full(AjaxMessageMeta(Full(title),
                                                         Full(styles.mkString(" ")))))
            case "warn" => MsgsWarningMeta(Full(AjaxMessageMeta(Full(title),
                                                           Full(styles.mkString(" ")))))
            case "notice" => MsgsNoticeMeta(Full(AjaxMessageMeta(Full(title),
                                                          Full(styles.mkString(" ")))))
          }
        }
        msg.toList.map(e => (<li>{e}</li>) ) match {
          case Nil => Nil
          case msgList => val ret = (<div id={LiftRules.noticesContainerId + "_" + ord}>{title}<ul>{msgList}</ul></div>)
            styles.foldLeft(ret)((xml, style) => xml % new UnprefixedAttribute("class", Text(style), Null))
        }
    }
    <div>{msgs}</div> % ("id" -> LiftRules.noticesContainerId)
  }
}

object MsgsNoticeMeta extends SessionVar[Box[AjaxMessageMeta]](Empty)
object MsgsWarningMeta extends SessionVar[Box[AjaxMessageMeta]](Empty)
object MsgsErrorMeta extends SessionVar[Box[AjaxMessageMeta]](Empty)

case class AjaxMessageMeta(title: Box[String], cssClass: Box[String])
