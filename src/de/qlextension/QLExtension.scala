package de.qlextension

import java.net.URLClassLoader
import java.net.URL
import java.io.File
import java.io.FileReader
import java.io.FileNotFoundException
import scala.collection.JavaConverters._
import org.nlogo.api.DefaultClassManager
import org.nlogo.api.PrimitiveManager
import org.nlogo.api.Primitive
import com.typesafe.config.ConfigFactory

/**
 * 
 * This class is the only element of ql.jar.
 * It initialises the ql-extension.
 * 
 * The main logic is implemented in the qlearning.jar.
 * 
 * This class is separated from the main logic because of problems regarding the class path.
 * If the additional jars are not in the system class loader, they must be reloaded when a HeadlessWorkspace loads the extension.
 * This leads to additional Akka ActorSystems, which impedes the communication between the HeadlessWorkspaces and main workspace (which loaded the extension).
 * 
 * There are two ways to prevent this behaviour:
 * 1. Adding the jars to the variable 'Class-Path' of the manifest file of NetLogo.jar.
 * 2. Adding the jars to the class path at runtime: http://stackoverflow.com/questions/1010919/adding-files-to-java-classpath-at-runtime
 * 
 * The latter is tried in the following. 
 * However, this is a hack that is not directly supported by Java. 
 * There might arise problems with a SecurityManager.
 * 
 * If problems arise, try the first way.
 * 
 */
object QLExtension {
  
  // name of NetLogo extension
  val EXTENSION_NAME = "ql"
  // name of section in configuration file
  val cfgstr = "netlogo"
  // path of configuration file
  val confFile = new File("extensions/ql/application.conf")
  // check whether confFile exists
  try {
    new FileReader(confFile)
  } catch {
    case e: FileNotFoundException =>
      System.err.println("FileNotFoundException: extensions/ql/application.conf") 
      exit(0)
  }
  
  // load configuration file
  val config = ConfigFactory.parseFile(confFile)
  val inParallelMode = config.getBoolean("netlogo.enable-parallel-mode")
    
  // additional jars that are needed
  val defaultJarList =  List[String]("extensions/ql/ql.jar",
                              "extensions/ql/qlearning.jar", 
                              "extensions/ql/akka-actor-2.0.5.jar",
                              "extensions/ql/akka-agent-2.0.5.jar",
                              "extensions/ql/config-1.0.2.jar", 
                              "extensions/ql/colt-1.2.0.jar", 
                              "extensions/ql/scala-stm_2.9.1-0.5.jar") 
                              
  val jarList = if (inParallelMode) defaultJarList ++ config.getStringList("netlogo.parallel.additional-jars").asScala else defaultJarList
  
  val sysloader = ClassLoader.getSystemClassLoader().asInstanceOf[URLClassLoader]
  
  if (inParallelMode) {                              
    // adding the jars to the system class loader
    val sysclass = classOf[URLClassLoader]
    try {
      val method = sysclass.getDeclaredMethod("addURL", classOf[URL])
      method.setAccessible(true)
      jarList.foreach(jarName => {
        val file = new File(jarName)
        // check whether jar exists
        new FileReader(file)
        // load jar
        method.invoke(sysloader, file.toURL())
      })
    } catch {
      case e: FileNotFoundException =>
        val newLine = System.getProperty("line.separator")
        System.err.println("FileNotFoundException: Check if all required jars exists: " + newLine +  
            jarList.tail.foldLeft(jarList.first)((s, el) => s + "," + newLine + el)) + "." + newLine + 
        exit(0)
      case t: Throwable => 
        val newLine = System.getProperty("line.separator")
        System.err.println("Setting additional jars failed. A SecurityManager may prevent the adding of jars to the class path at runtime." + newLine + 
            "Manually add the names of the jars to the variable 'Class-Path' of the manifest file of NetLogo.jar.")
        t.printStackTrace()
    }
  } 
}

/**
 * the extension class needed by NetLogo
 */
class QLExtension extends DefaultClassManager {
  import QLExtension._
  
  private def getPrimitive(name: String) = {
    sysloader.loadClass(name).getConstructor().newInstance().asInstanceOf[Primitive]
  }
  
  override def load(manager: PrimitiveManager) {
    manager.addPrimitive("init", getPrimitive("de.qlearning.Init"))
    manager.addPrimitive("create-group", getPrimitive("de.qlearning.CreateGroup"))
    manager.addPrimitive("set-group-structure", getPrimitive("de.qlearning.NewGroupStructure"))
    manager.addPrimitive("start", getPrimitive("de.qlearning.Start"))
    manager.addPrimitive("stop", getPrimitive("de.qlearning.Stop"))
    manager.addPrimitive("decay-exploration", getPrimitive("de.qlearning.DecreaseExperimenting"))
    manager.addPrimitive("get-group-list", getPrimitive("de.qlearning.GetGroupList"))
    manager.addPrimitive("get-agents", getPrimitive("de.qlearning.GetAgents"))
    manager.addPrimitive("get-decisions", getPrimitive("de.qlearning.GetDecisions"))
    manager.addPrimitive("set-rewards", getPrimitive("de.qlearning.SetRewards"))
    manager.addPrimitive("set-new-states", getPrimitive("de.qlearning.SetNewStates"))
    manager.addPrimitive("get-performance", getPrimitive("de.qlearning.GetPerformance"))
    
    manager.addPrimitive("one-of", getPrimitive("de.qlearning.OneOf"))
    manager.addPrimitive("set-reward", getPrimitive("de.qlearning.SetReward"))
    manager.addPrimitive("set-reward-and-state", getPrimitive("de.qlearning.SetRewardAndState"))
  }
    
  
  override def additionalJars: java.util.List[String] = jarList.asJava
  
  override def clearAll() {
    // nothing to do
  }
    
}