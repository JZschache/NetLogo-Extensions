package de.qlextension

import org.nlogo.headless.HeadlessWorkspace
import org.nlogo.agent.World
import org.nlogo.nvm.CompilerInterface
import org.nlogo.api.RendererInterface
import org.nlogo.api.AggregateManagerInterface
import org.nlogo.workspace.AbstractWorkspace
import org.nlogo.workspace.ExtensionManager
import java.net.URL
import java.net.URLClassLoader
import java.io.File

class MyClassLoader(urls: Array[URL]) extends URLClassLoader(urls){
  
    def addAnURL(url:URL) {
        super.addURL(url)
    }
  
  }

class MyHeadlessWorkspace(
  _world: World,
  compiler: CompilerInterface,
  renderer: RendererInterface,
  aggregateManager: AggregateManagerInterface,
  hubNetManagerFactory: AbstractWorkspace.HubNetManagerFactory) 
extends HeadlessWorkspace(
  _world, compiler, renderer, aggregateManager, hubNetManagerFactory) {
  
  val jar1ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/qlearning.jar");
  val jar2ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/akka-actor-2.0.5.jar");
  val jar3ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/akka-agent-2.0.5.jar");
  val jar4ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/config-1.0.2.jar");
  val jar5ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/colt-1.2.0.jar");
  val jar6ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/scala-stm_2.9.1-0.5.jar");
  
  
  println("MyHeadlessWorkspace")
  
  println("System class loader")
  val loader = new MyClassLoader(ClassLoader.getSystemClassLoader().asInstanceOf[URLClassLoader].getURLs())
  loader.getURLs().foreach(p => println(p))
  
  loader.addAnURL(jar1ToAdd.toURL())
  loader.addAnURL(jar2ToAdd.toURL())
  loader.addAnURL(jar3ToAdd.toURL())
  loader.addAnURL(jar4ToAdd.toURL())
  loader.addAnURL(jar5ToAdd.toURL())
  loader.addAnURL(jar6ToAdd.toURL())
  
  println("MyHeadlessWorkspace class loader")
  val loader2 = new MyClassLoader(this.getClass().getClassLoader().asInstanceOf[URLClassLoader].getURLs())
  
  loader2.getURLs().foreach(p => println(p))
  
  
  
}

class MyExtensionManager(workspace: AbstractWorkspace) extends ExtensionManager(workspace) {
  
}