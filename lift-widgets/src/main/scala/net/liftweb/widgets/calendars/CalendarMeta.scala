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
  
package net.liftweb.widgets.calendar;

import java.util.Locale
import java.util.Calendar._

case class CalendarMeta(firstDayOfWeek: Int, locale: Locale) {
  val cellHeadOtherMonth = "cellHeadOtherMonth"
  val cellBodyOtherMonth = "cellBodyOtherMonth"
  val cellHead = "cellHead"
  val cellBody = "cellBody"
  val cellWeek = "cellWeek"
  val monthView = "monthView"
  val topHead = "topHead"
  val calendarItem = "calendarItem"
  val cellHeadToday = "cellHeadToday"
  val cellBodyToday = "cellBodyToday"
}