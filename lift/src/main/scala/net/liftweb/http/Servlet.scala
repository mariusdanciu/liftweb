package net.liftweb.http;

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import javax.servlet.{ServletContext}
import scala.collection.mutable.{ListBuffer}
import java.net.URLDecoder
import scala.xml.{Node, NodeSeq,Group, Elem, MetaData, Null, UnprefixedAttribute, XML, Comment}
import scala.xml.transform._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import java.io.InputStream
import net.liftweb.util.Helpers
import net.liftweb.util.ActorPing
import net.liftweb.sitemap.SiteMap
import java.net.URL
import net.liftweb.sitemap._

/**
 * An implementation of HttpServlet.  Just drop this puppy into 
 * your Java web container, do a little magic in web.xml, and
 * ta-da, you've got a scala-powered Servlet
 * 
 */
class Servlet extends HttpServlet {
  private val actorNameConst = "the_actor"
  private var requestCnt = 0
  private val selves = new ListBuffer[Actor]
  
  override def destroy = {
    try {
    Servlet.ending = true
    this.synchronized {
      while (requestCnt > 0) {
        selves.foreach(s => s ! None)
        wait(100L)
      }
    }
    Scheduler.snapshot // pause the Actor scheduler so we don't have threading issues
    ActorPing.snapshot
    Log.debug("Destroyed servlet")
    super.destroy
    } catch {
      case e => Log.error("Servlet destruction failure",e)
    }
  }
  
  override def init = {
    if (Scheduler.tasks ne null) {Log.debug("Restarting Scheduler"); Scheduler.restart} // restart the Actor scheduler
    ActorPing.start
    Servlet.ending = false
    super.init
  }
  
  /**
   * Forward the GET request to the POST handler
   */
  def doGet(request : HttpServletRequest , response : HttpServletResponse, start: Long) = {
    isExistingFile_?(request).map(u => doServiceFile(u, request, response)) openOr
      doService(request, response, RequestType(request ), start)
  }

  /**
   * Is the file an existing file in the WAR?
   */
  private def isExistingFile_?(request : HttpServletRequest) : Can[URL] = {
    if (!goodPath_?(request.getRequestURI)) Empty else
    getServletContext.getResource(request.getRequestURI.substring(request.getContextPath.length)) match {
      case null => Empty
      case u : URL => Full(u)
      case _ => Empty
    }
  }
  
  private def doServiceFile(url: URL, request : HttpServletRequest , response : HttpServletResponse) {
    val uc = url.openConnection
    val mod = request.getHeader("if-modified-since")
    if (mod != null) {
      val md = parseInternetDate(mod)
      if (uc.getLastModified <= md.getTime) {
        response.sendError(304)
        return
      }
    }
    val in = uc.getInputStream
    
    try {
    val li = request.getRequestURI.lastIndexOf('.')
    if (li != -1) {
      response.setContentType(request.getRequestURI.substring(li + 1) match {
        case "js" => "text/javascript"
        case "css" => "text/css"
        case "png" => "image/png"
        case "gif" => "image/gif"
        case "ico" => "image/x-icon"
        case _ => "text/html"
      })
    }
    
    response.setDateHeader("Last-Modified", uc.getLastModified)

    val out = response.getOutputStream
    val ba = new Array[byte](2048)

    def readAndWrite {
      val len = in.read(ba)
      if (len >= 0) {
        if (len > 0) {
          out.write(ba, 0, len)
        }
        readAndWrite
      }
    }
    
    // a "while-less" read and write loop
    readAndWrite
    
    out.flush
    } finally {
      in.close
    }
  }
  
  
  def getActor(request: RequestState, session: HttpSession) = {
    session.getValue(actorNameConst) match {
      case r : Session => r
      case _ => 
        val ret = Session(request.uri, request.path, request.contextPath, request.requestType, request.webServices_?,
            request.contentType)
        ret.start
        session.putValue(actorNameConst, ret)
        ret
    }
  }
  
  override def service(req: HttpServletRequest,resp: HttpServletResponse) {
    try {
    def doIt {
      logTime("Service request "+req.getRequestURI) {
        Servlet.early.foreach(_(req))
        Servlet.setContext(getServletContext)
        val start = System.nanoTime
        req.getMethod.toUpperCase match {
          case "GET" => doGet(req, resp, start)
          case _ => doService(req, resp, RequestType(req), start)
        }
        }      
    }
    Servlet.checkJetty(req) match {
      case None => doIt
      case r if r eq null => doIt
      case r: ResponseIt => sendResponse(r.toResponse, resp, Empty)
      case Some(r: ResponseIt) => sendResponse(r.toResponse, resp, Empty)
          case _ => doIt
  }
    } catch {
      case e => Log.warn("Request for "+req.getRequestURI+" failed "+e.getMessage, e); throw new Exception("Request failed", e)
    }
  }
  
  /**
   * Service the HTTP request
   */ 
  def doService(request:HttpServletRequest , response: HttpServletResponse, requestType: RequestType, start: Long) {
    val session = RequestState(request, Servlet.rewriteTable(request), getServletContext, start)

    val toMatch = RequestMatcher(session, session.path)
    
      val resp: Response = if (Servlet.ending) {
        session.createNotFound.toResponse
      } else if (Servlet.dispatchTable(request).isDefinedAt(toMatch)) {
        val sessionActor = getActor(session, request.getSession)
         
	S.init(session, sessionActor, new VarStateHolder(sessionActor, sessionActor.currentVars, Empty, false)) {
	  val f = Servlet.dispatchTable(request)(toMatch)
	  f(request) match {
            case Full(v) => Servlet.convertResponse(v, session)
            case Empty => session.createNotFound.toResponse
            case f: Failure => session.createNotFound(f).toResponse 
	  }
	}
      } else {
	
        val sessionActor = getActor(session, request.getSession)
	
        try {
          this.synchronized {
            this.requestCnt = this.requestCnt + 1
            self.trapExit = true
            selves += self
          }
          
        def drainTheSwamp { // remove any message from the current thread's inbox
          receiveWithin(0) {
            case TIMEOUT => true
            case s @ _ => Log.trace("Drained "+s) ; false
          } match {
            case false => drainTheSwamp
            case _ =>
          }
        }
        
        drainTheSwamp
        
        val timeout = (Servlet.calcRequestTimeout.map(_(session)) openOr (if (session.ajax_?) (Servlet.ajaxRequestTimeout openOr 120) 
            else (Servlet.stdRequestTimeout openOr 10))) * 1000L        
        
        if (session.ajax_? && Servlet.hasJetty_?) {
          sessionActor ! AskSessionToRender(session, request, 120000L, a => Servlet.resumeRequest(a, request))
          Servlet.doContinuation(request)
        } else {
	
        val thisSelf = self
	sessionActor ! AskSessionToRender(session, request, timeout, a => thisSelf ! a)
        receiveWithin(timeout) {
          case AnswerHolder(r) => r.toResponse
          // if we failed allow the optional handler to process a request 
	  case n @ TIMEOUT => Servlet.requestTimedOut.flatMap(_(session, n)) match {
            case Full(r) => r
            case _ => Log.warn("Got unknown (Servlet) resp "+n); session.createNotFound.toResponse
          }
	}
        }
        } finally {
          this.synchronized {
            this.requestCnt = this.requestCnt - 1
            selves -= self
            this.notifyAll
          }
        }
      }
    
    sendResponse(resp, response, Full(session))
  }
  
  def sendResponse(resp: Response, response: HttpServletResponse, request: Can[RequestState]) {
    val bytes = resp.data
    val len = bytes.length
    // insure that certain header fields are set
    val header = insureField(resp.headers, List(("Content-Type", "text/html"),
                                                ("Content-Encoding", "UTF-8"),
                                                ("Content-Length", len.toString)));
    
    Servlet._beforeSend.foreach(_(resp, response, header, request))
    
    // send the response
    header.elements.foreach {case (name, value) => response.setHeader(name, value)}
    response setStatus resp.code
    response.getOutputStream.write(bytes)    
    Servlet._afterSend.foreach(_(resp, response, header, request))
  }
}

object Servlet {
  val SessionDispatchTableName = "$lift$__DispatchTable__"
  val SessionRewriteTableName = "$lift$__RewriteTable__"
  val SessionTemplateTableName = "$lift$__TemplateTable__"
    
  type DispatchPf = PartialFunction[RequestMatcher, HttpServletRequest => Can[ResponseIt]];
  type RewritePf = PartialFunction[RewriteRequest, RewriteResponse]
  type TemplatePf = PartialFunction[RequestMatcher,() => Can[NodeSeq]]
  type SnippetPf = PartialFunction[List[String], NodeSeq => NodeSeq]
             
  private var _early: List[(HttpServletRequest) => Any] = Nil
  private[http] var _beforeSend: List[(Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any] = Nil
  
  def appendBeforeSend(f: (Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any) {
    _beforeSend = _beforeSend ::: List(f)
  }
  
  private[http] var _afterSend: List[(Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any] = Nil
  
  def appendAfterSend(f: (Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any) {
    _afterSend = _afterSend ::: List(f)
  }
  
  /**
    * Put a function that will calculate the request timeout based on the
    * incoming request.
    */
  var calcRequestTimeout: Can[RequestState => Int] = Empty
  
  /**
    * If you want the standard (non-AJAX) request timeout to be something other than
    * 10 seconds, put the value here
    */
  var stdRequestTimeout: Can[Int] = Empty
  
  /**
    * If you want the AJAX request timeout to be something other than 120 seconds, put the value here
    */
  var ajaxRequestTimeout: Can[Int] = Empty
  
  /**
    * If the request times out (or returns a non-Response) you can
    * intercept the response here and create your own response
    */
  var requestTimedOut: Can[(RequestState, Any) => Can[Response]] = Empty
  
  def early = {
    test_boot
    _early
  }
  
  val (hasJetty_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
    val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
    val meth = cc.getMethod("getContinuation", Array(classOf[HttpServletRequest], classOf[AnyRef]))
    val cci = Class.forName("org.mortbay.util.ajax.Continuation")
    val getObj = cci.getMethod("getObject", null)
    val setObj = cci.getMethod("setObject", Array(classOf[AnyRef]))
    val suspend = cci.getMethod("suspend", Array(java.lang.Long.TYPE))
    val resume = cci.getMethod("resume", null)
    (true, (cc), (meth), (getObj), (setObj), (suspend), resume)
    } catch {
      case e => (false, null, null, null, null, null, null)
    }
  }
  
  def resumeRequest(what: AnyRef, req: HttpServletRequest) {
    val cont = getContinuation.invoke(contSupport, Array(req, Servlet))
    setObject.invoke(cont, Array(what))
    resume.invoke(cont, null)
  }
  
  def doContinuation(req: HttpServletRequest): Nothing = {
    try {
    val cont = getContinuation.invoke(contSupport, Array(req, Servlet))
    Log.trace("About to suspend continuation")
    suspend.invoke(cont, Array(new java.lang.Long(200000L)))
    throw new Exception("Bail")
    } catch {
      case e: java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") => throw e.getCause
    }
  }
  
  def checkJetty(req: HttpServletRequest) = {
    if (!hasJetty_?) None
    else {
      val cont = getContinuation.invoke(contSupport, Array(req, Servlet))
      val ret = getObject.invoke(cont, null)
      setObject.invoke(cont, Array(null))
      ret
    }
  }
  
  private var _sitemap: Can[SiteMap] = Empty
  
  def setSiteMap(sm: SiteMap) {_sitemap = Full(sm)}
  def siteMap: Can[SiteMap] = _sitemap
    
  def appendEarly(f: HttpServletRequest => Any) = _early = _early ::: List(f)
  
  var ending = false
  private case class Never
  
  private def rpf[A,B](in: List[PartialFunction[A,B]], last: PartialFunction[A,B]): PartialFunction[A,B] = in match {
    case Nil => last
    case x :: xs => x orElse rpf(xs, last)
  }
  
  def dispatchTable(req: HttpServletRequest): DispatchPf = {
    test_boot
    req.getSession.getAttribute(SessionDispatchTableName) match {
      case null | Nil  => dispatchTable_i 
      case dt: List[S.DispatchHolder] => rpf(dt.map(_.dispatch), dispatchTable_i)
      case _ => dispatchTable_i
    }
  }
  
  def rewriteTable(req: HttpServletRequest): RewritePf = {
    test_boot
    req.getSession.getAttribute(SessionRewriteTableName) match {
      case null | Nil => rewriteTable_i
    case rt: List[S.RewriteHolder] => rpf(rt.map(_.rewrite), rewriteTable_i)
    case _ => rewriteTable_i
  }
  }
  
  def snippetTable: SnippetPf = snippetTable_i
  
  def templateTable: TemplatePf = {
    S.sessionTemplater match {
      case Nil => templateTable_i
      case rt => rpf(rt.map(_.template), templateTable_i)
    }
  }

  private var _context: ServletContext = _
  
  def context: ServletContext = synchronized {_context}
  
  def setContext(in: ServletContext): unit =  synchronized {
    if (in ne _context) {
      Helpers.setResourceFinder(in.getResource)
      _context = in
      }
  }
  
  private var dispatchTable_i : DispatchPf = Map.empty
  
  private var rewriteTable_i : RewritePf = Map.empty
  
  private var templateTable_i: TemplatePf = Map.empty
  
  private var snippetTable_i: SnippetPf = Map.empty
  
  private val test_boot = {
    try {
      val c = Class.forName("bootstrap.liftweb.Boot")
      val i = c.newInstance
      val f = createInvoker("boot", i)
      f.map{f => f()}
      
    } catch {
    case e: java.lang.reflect.InvocationTargetException => Log.error("Failed to Boot", e); None
    case e => Log.error("Failed to Boot", e); None
    }
  }
  
  def addSnippetBefore(pf: SnippetPf) = {
    snippetTable_i = pf orElse snippetTable_i
    snippetTable_i
  }
  
  def addSnippetAfter(pf: SnippetPf) = {
    snippetTable_i = snippetTable_i orElse pf
    snippetTable_i
  }
  
  def addTemplateBefore(pf: TemplatePf) = {
    templateTable_i = pf orElse templateTable_i
    templateTable_i
  }

  def addTemplateAfter(pf: TemplatePf) = {
    templateTable_i = templateTable_i orElse pf
    templateTable_i
  }

  def addRewriteBefore(pf: RewritePf) = {
    rewriteTable_i = pf orElse rewriteTable_i
    rewriteTable_i
  }
  
  def addRewriteAfter(pf: RewritePf) = {
    rewriteTable_i = rewriteTable_i orElse pf
    rewriteTable_i
  }
  
  def addDispatchBefore(pf: DispatchPf) = {
    dispatchTable_i = pf orElse dispatchTable_i
    dispatchTable_i
  }
  
  def addDispatchAfter(pf: DispatchPf) = {
    dispatchTable_i = dispatchTable_i orElse pf
    dispatchTable_i
  }
  
  def convertResponse(r: Any, session: RequestState): Response = {
    r match {
      case r: ResponseIt => r.toResponse
      case ns: NodeSeq => convertResponse(XhtmlResponse(Group(session.fixHtml(Group(ns))), ResponseInfo.xhtmlTransitional, Nil, 200), session)
      case Some(o) => convertResponse(o, session)
      case _ => session.createNotFound.toResponse
    }
  }
}

