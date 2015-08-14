package de.qlextension

import java.net.URLClassLoader
import java.net.URL
import java.io.File

import org.nlogo.api.DefaultClassManager
import org.nlogo.api.PrimitiveManager
import org.nlogo.api.Primitive


object QLExtension {
  
  println("adding qlearning.jar to classpath")
  
  val jar1ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/qlearning.jar");
  val jar2ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/akka-actor-2.0.5.jar");
  val jar3ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/akka-agent-2.0.5.jar");
  val jar4ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/config-1.0.2.jar");
  val jar5ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/colt-1.2.0.jar");
  val jar6ToAdd = new File("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/scala-stm_2.9.1-0.5.jar");
  
  class MyClassLoader(urls: Array[URL]) extends URLClassLoader(urls){
  
    def addAnURL(url:URL) {
        super.addURL(url)
    }
  
  }
  
  val loader = new MyClassLoader(ClassLoader.getSystemClassLoader().asInstanceOf[URLClassLoader].getURLs())
  
  println("System class loader from QLExtension object")
  loader.getURLs().foreach(p => println(p))
  
  loader.addAnURL(jar1ToAdd.toURL())
  loader.addAnURL(jar2ToAdd.toURL())
  loader.addAnURL(jar3ToAdd.toURL())
  loader.addAnURL(jar4ToAdd.toURL())
  loader.addAnURL(jar5ToAdd.toURL())
  loader.addAnURL(jar6ToAdd.toURL())
  
    
}

/**
 * the extension class needed by NetLogo
 */
class QLExtension extends DefaultClassManager {
  import QLExtension._
  
  import akka.agent.{ Agent => AkkaAgent }
  
  jar1ToAdd
  
  println("QLExtension class loader")
  val loader2 = new MyClassLoader(this.getClass().getClassLoader().asInstanceOf[URLClassLoader].getURLs())
  
  loader2.getURLs().foreach(p => println(p))
  
  val loader3 = new MyClassLoader(ClassLoader.getSystemClassLoader().asInstanceOf[URLClassLoader].getURLs())
  println("System class loader from QLExtension class")
  loader3.getURLs().foreach(p => println(p))
  loader3.addAnURL(jar1ToAdd.toURL())
  loader3.addAnURL(jar2ToAdd.toURL())
  loader3.addAnURL(jar3ToAdd.toURL())
  loader3.addAnURL(jar4ToAdd.toURL())
  loader3.addAnURL(jar5ToAdd.toURL())
  loader3.addAnURL(jar6ToAdd.toURL())
  loader3.getURLs().foreach(p => println(p))
  
  override def load(manager: PrimitiveManager) {
    // observer primitives
    manager.addPrimitive("init", loader.loadClass("de.qlearning.Init").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("create-group", loader.loadClass("de.qlearning.CreateGroup").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("set-group-structure", loader.loadClass("de.qlearning.NewGroupStructure").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("start", loader.loadClass("de.qlearning.Start").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("stop", loader.loadClass("de.qlearning.Stop").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("decay-exploration", loader.loadClass("de.qlearning.DecreaseExperimenting").getConstructor().newInstance().asInstanceOf[Primitive])
    
    manager.addPrimitive("get-group-list", loader.loadClass("de.qlearning.GetGroupList").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("get-agents", loader.loadClass("de.qlearning.GetAgents").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("get-decisions", loader.loadClass("de.qlearning.GetDecisions").getConstructor().newInstance().asInstanceOf[Primitive])
    manager.addPrimitive("set-rewards", loader.loadClass("de.qlearning.SetRewards").getConstructor().newInstance().asInstanceOf[Primitive])
    
    manager.addPrimitive("get-performance", loader.loadClass("de.qlearning.GetPerformance").getConstructor().newInstance().asInstanceOf[Primitive])
  }
    
  
  override def additionalJars: java.util.List[String] = {
    val list : java.util.List[String] =  new java.util.ArrayList[String]
    list.add("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/qlearning.jar")
    list.add("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/akka-actor-2.0.5.jar")
    list.add("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/akka-agent-2.0.5.jar")
    list.add("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/config-1.0.2.jar")
    list.add("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/colt-1.2.0.jar")
    list.add("/home/johannes/netlogo/netlogo-5.2.0/extensions/ql/hiddenlibs/scala-stm_2.9.1-0.5.jar")
    list
  }
  
  override def clearAll() {
    //TODO:
//    import QLSystem._
    // stop all data agents and reset the map
    // this may take a while
//    qlDataMap send {map => {
//      map.values.foreach(_.close)
//      Map[org.nlogo.api.Agent, AkkaAgent[QLAgent]]()
//    }}
    // wait till complete
//    qlDataMap.await
  }
    
}