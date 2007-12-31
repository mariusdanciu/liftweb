package net.liftweb.webapptest

import org.mortbay.jetty.Server
//import org.mortbay.jetty.servlet.Context
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext

import net.sourceforge.jwebunit.junit.WebTester
import junit.framework.AssertionFailedError

/*
object JettyTestServer {
  private val serverPort_ = System.getProperty("SERVLET_PORT", "8989").toInt
  private var baseUrl_ = "http://localhost:" + serverPort_

  private val server_ : Server = {
    val server = new Server(serverPort_)
    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    context.setWar("src/test/webapp")        
    //val context = new Context(_server, "/", Context.SESSIONS)
    //context.addFilter(new FilterHolder(new LiftFilter()), "/");
    server.addHandler(context)
    server
  }

  def urlFor(path: String) = baseUrl_ + path

  def start() = server_.start()
    
  def stop() = {
    server_.stop()
    server_.join()
  }
  
  def browse(startPath: String, f:(WebTester) => Unit) = {
    val wc = new WebTester()
    try {
      wc.setScriptingEnabled(false)
      wc.beginAt(JettyTestServer.urlFor(startPath))      
      f(wc)
    } catch {
      case exc: AssertionFailedError => {
        System.err.println("serveur response: ", wc.getServeurResponse())
        throw exc
      }
    } finally {
      wc.closeBrowser()
    }  
  }

}

*/
