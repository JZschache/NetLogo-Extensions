package de.qlearning

import java.util.{ List => JList }
import java.io.File
import scala.collection.JavaConverters._
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import akka.routing.FromConfig
import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._
import com.typesafe.config.ConfigFactory
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import akka.agent.{ Agent => AkkaAgent }
import akka.routing.Broadcast
import akka.dispatch.ExecutionContext
import akka.routing.SmallestMailboxRouter


object QLSystem {
  
  val confFile = "conf/application.conf"
  val system = ActorSystem("QLSystem", ConfigFactory.parseFile(new File(confFile)))
  
//  val config = system.settings.config
//  val pbc = config.getInt(cfgstr + ".pause_between_choice_ms")
  
//  val alteredData = AkkaAgent(List[(Agent,String,Double)]())(system)
  // the guiActor gets an own Thread-Pool for optimal performance
//  val nlguiActor = system.actorOf(Props(new NetLogoGUIActor(App.app)).withDispatcher("netlogo-dispatcher"), "NetLogoGUIActor")
  
//  val noOfEnv = if(conHeadEnv == 0) 1 else conHeadEnv
//  val r = (1 to noOfEnv).map(i => (i -> AkkaAgent[List[String]](List())(system))).toMap
//  val r = (1 to conHeadEnv).map(i => (i -> AkkaAgent[List[String]](List())(system))).toMap
  
  // the helper keeps track of parameters that are set by an NetLogoHeadlessActor and 
  // used from an NetLogoHeadlessApp in reward function calls
  val rewProcHelper: RewardProcedureHelper = new RewardProcedureHelper(system)
  val conHeadlessEnv = system.settings.config.getInt("netlogo.concurrent-headless-environments")
  val routeesMap = (1 to conHeadlessEnv).map(id => {
    (id -> system.actorOf(Props(new NetLogoHeadlessActor(id)).withDispatcher(NetLogoActors.dispatcherName)))
  }).toMap
  val netLogoRouter = system.actorOf(Props[NetLogoHeadlessActor].withRouter(SmallestMailboxRouter(routeesMap.values)), NetLogoActors.routerName)
  val netLogoSuper = system.actorOf(Props(new NetLogoSupervisor(netLogoRouter)).withDispatcher(NetLogoActors.dispatcherName), NetLogoActors.supervisorName)
  
  
  val QLEXTENSION = "ql"
    
  // index of the (NetLogo-)agent's variables-array that holds the QLAgent 
//  var agentMappingIndex = 0
//  val getActorRef = (nlAgent: org.nlogo.api.Agent) => {
//    nlAgent.getVariable(agentMappingIndex).asInstanceOf[ActorRef]
//  }
    
//  var agentMap = Map[org.nlogo.api.Agent, ActorRef]()
//  var agents = List[ActorRef]()
//  var groupList = List[ActorRef]()
//  var groupAgentsMap = Map[Int,List[Agent]]()
//  var environmentList = List[ActorRef]()
//  var envIndex = -1
    //TODO: should be an AkkaAgent
  var qlDataMap = Map[Agent, AkkaAgent[QLAgent]]()  
  
  def init() {
    
  }
}

class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    // observer primitives
    manager.addPrimitive("init", new Init)
    manager.addPrimitive("set-group-structure", new SetGroupStructure)
    manager.addPrimitive("start", new Start)
    manager.addPrimitive("stop", new Stop)
    manager.addPrimitive("get-decisions", rewProcHelper)
    manager.addPrimitive("decrease-experimenting", new DecreaseExperimenting)
    manager.addPrimitive("create-singleton", new CreateSingleton)
    manager.addPrimitive("create-group", new CreateGroup)
    // agent primitives
    manager.addPrimitive("get-q-value", new GetQValue)
    manager.addPrimitive("get-last-choice", new GetLastChoice)
    manager.addPrimitive("get-total-n", new GetTotalN)
    manager.addPrimitive("get-n", new GetN)
  }
  
  override def additionalJars: JList[String] = {
    val list : java.util.List[String] =  new java.util.ArrayList[String]
    list.add("akka-actor-2.0.5.jar")
    list.add("akka-agent-2.0.5.jar")
    list.add("config-1.0.2.jar")
    list.add("colt-1.2.0.jar")
    list.add("scala-stm_2.9.1-0.5.jar")
    list
  }
  
  override def clearAll() {
//    environmentList.foreach(env => system.stop(env))
//    environmentList = List[ActorRef]()
//    envIndex = -1
//    groupList.foreach(group => system.stop(group))
//    groupList = List[ActorRef]()
//    groupAgentsMap = Map[Int, List[Agent]]()
//    agentMap.values.foreach(agent => system.stop(agent))
//    agentMap = Map[Agent, ActorRef]()
//    agents.foreach(agent => system.stop(agent))
//    agents = List[ActorRef]()
    qlDataMap.values.foreach(_.close)
    qlDataMap = Map[org.nlogo.api.Agent, AkkaAgent[QLAgent]]()
  }
    
}

class NLGroup(val nlAgents: List[org.nlogo.api.Agent], val qlAgents: List[AkkaAgent[QLAgent]], val alternatives: List[(AkkaAgent[QLAgent], List[String])]) extends org.nlogo.api.ExtensionObject {
    
  /**
   * @param readable  If true the result should be readable as NetLogo code
   * @param exporting If false the result is for display only
   * @param reference If true the result may be a reference to a complete object exported in the extension section of the file if false the object should be recreatable from the result
   * @return a string representation of the object.
   */
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = 
//    group.foldLeft("".toString())((s, pair) => s + "( " + pair._1.id + " : " + 
//        pair._3.tail.foldLeft(pair._3.head)((subs, alt) => subs + ", " + alt) + ")")
    nlAgents.foldLeft("".toString())((s, a) => s + " " + a.id + " ")

  /** @return the name of the extension this object was created by */
  def getExtensionName: String = QLSystem.QLEXTENSION

  /**
   * @return the type of this Object, which is extension defined.
   *         If this is the only ExtensionObject type defined by this extension
   *         it is appropriate to return an empty string.
   */
  def getNLTypeName: String = "NLGroup"

  /**
   * @return true if this object equal to obj
   *         not simply the same object but all of the
   *         elements are the same
   */
  def recursivelyEqual(obj: AnyRef): Boolean = {
    obj.isInstanceOf[NLGroup] && obj.asInstanceOf[NLGroup].nlAgents.equals(this.nlAgents)
  }
    
}

class NLGroupChoice(val nlAgents: List[org.nlogo.api.Agent], val qlAgents: List[AkkaAgent[QLAgent]], val choices: List[String], val results: List[Double]) extends org.nlogo.api.ExtensionObject {
  
  /**
   * @param readable  If true the result should be readable as NetLogo code
   * @param exporting If false the result is for display only
   * @param reference If true the result may be a reference to a complete object exported in the extension section of the file if false the object should be recreatable from the result
   * @return a string representation of the object.
   */
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = 
//    group.foldLeft("".toString())((s, pair) => s + "( " + pair._1.id + " : " + 
//        pair._3.tail.foldLeft(pair._3.head)((subs, alt) => subs + ", " + alt) + ")")
    nlAgents.foldLeft("".toString())((s, a) => s + " " + a.id + " ")

  /** @return the name of the extension this object was created by */
  def getExtensionName: String = QLSystem.QLEXTENSION

  /**
   * @return the type of this Object, which is extension defined.
   *         If this is the only ExtensionObject type defined by this extension
   *         it is appropriate to return an empty string.
   */
  def getNLTypeName: String = "NLGroupChoice"

  /**
   * @return true if this object equal to obj
   *         not simply the same object but all of the
   *         elements are the same
   */
  def recursivelyEqual(obj: AnyRef): Boolean = {
    obj.isInstanceOf[NLGroupChoice] && obj.asInstanceOf[NLGroupChoice].nlAgents.equals(this.nlAgents)
  }
    
}

class RewardProcedureHelper(system: ActorSystem) extends DefaultReporter {
  
//  val helper = system.actorOf(Props[NetLogoHelperActor].withDispatcher(NetLogoActors.dispatcherName), NetLogoActors.helperName)
  
//  implicit val ec = system.dispatchers.lookup("my-dispatcher")
  
//  def getLargestKey = (0 /: envParameterMap.keys) (Math.max(_,_))
  
//  def setParameter(envId: Int, agent: AkkaAgent[List[(org.nlogo.api.Agent,String)]]) {
//    envParameterMap = envParameterMap.updated(envId, agent)
//  }
  
  override def getAgentClassString = "O"
  
  override def getSyntax = reporterSyntax(Array[Int](NumberType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
//    println("rewardProcedureHelperCall: " + args(0).getIntValue)
    implicit val timeout = Timeout(5 seconds)
    implicit val ec = system.dispatcher
    val future = (QLSystem.routeesMap(args(0).getIntValue) ? NetLogoActors.GetGroupChoice).mapTo[NetLogoActors.GroupChoice]
    //TODO: coding the future
//    val param = Await.result(future, 5 seconds).param
//    println("params reveived: " + param)
//    val param = envParameterMap(args(0).getIntValue).await(Timeout(5.seconds))
    return param.map(pair => List(pair._1, pair._2).toLogoList).toLogoList
  }
  
}


class Init extends DefaultCommand {
  import QLSystem._
//  import StartNetLogo._
//  import QLAgent._
  
  override def getAgentClassString = "O"
  
  // takes a turtle- / patchset, the experimenting and the exploration global
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, NumberType, StringType))
  
  /**
   * new QLAgents are generated for the turtles / patches
   * they are initialised with the environment and added to the agentMap
   */
  def perform(args: Array[Argument], c: Context) {
    
    netLogoSuper ! NetLogoActors.InitNetLogoActors
    
    val newNLAgents = args(0).getAgentSet.agents.asScala
//    val altList = try {
//      args(1).getList.toList.map(ar => ar.asInstanceOf[String])
//    } catch {
//      case ex:ClassCastException =>   
//        args(1).getList.toList.map(ar => ar.asInstanceOf[Double].toString)
//    }
    val experimenting = args(1).getDoubleValue
    val exploration = args(2).getString
    
    // set index of variables-array that holds the QLAgent 
//    agentMappingIndex = if (newAgents.isEmpty) 0 else newAgents.first.variables.length   
      
//    agentMap = newAgents.map(nlAgent => {
//      // create a QLData-object that holds all the information about the Q-learning process
//      val qlDataAgent = AkkaAgent(QLAgent.QLData())(system)
//      qlDataMap = qlDataMap.updated(nlAgent, qlDataAgent)
//      // create a QLAgent
//      val a = if (nlAgent.isInstanceOf[org.nlogo.api.Turtle]) {
//        system.actorOf(Props(new QLAgent(qlDataAgent, experimenting, exploration)), "QLAgent-" + nlAgent.getVariable(0))
//      } else { // is patch
//        system.actorOf(Props(new QLAgent(qlDataAgent, experimenting, exploration)), "QLAgent-" + nlAgent.getVariable(0) + "-" + nlAgent.getVariable(1))
//      }
//      nlAgent -> a
//    }).toMap
    
    // create a QLData-object that holds all the information about the Q-learning process
    qlDataMap = newNLAgents.map(_ -> AkkaAgent(QLAgent(exploration, experimenting))(system)).toMap
    
  }
    
}


class CreateSingleton extends DefaultReporter {
  override def getAgentClassString = "OTPL"
  override def getSyntax = reporterSyntax(Array( TurtleType | PatchType, ListType), WildcardType)
  
  def report(args: Array[Argument], context: Context): AnyRef = {
    val agent = args(0).getAgent
    val alternatives = args(1).getList.map(_.asInstanceOf[String]).toList
//    new NLGroup(List((agent, QLSystem.agentMap(agent), alternatives)))
    new NLGroup(List(agent), List((QLSystem.qlDataMap(agent), alternatives)))
  }
  
}

class CreateGroup extends DefaultReporter {
  override def getAgentClassString = "OTPL"
  override def getSyntax = reporterSyntax(Array( TurtlesetType | PatchsetType, ListType), WildcardType)
  
  def report(args: Array[Argument], context: Context): AnyRef = {
    val agents = args(0).getAgentSet.agents.asScala.toList
    val alternatives = args(1).getList.map(_.asInstanceOf[String]).toList
//    new NLGroup(agents.map(a => (a, QLSystem.agentMap(a), alternatives)).toList) 
    new NLGroup(agents, agents.map(a => (QLSystem.qlDataMap(a), alternatives)))
  }
  
}

class SetGroupStructure extends DefaultCommand {
  import QLSystem._
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array(ListType))
  
  def perform(args: Array[Argument], c: Context) {
    
    val groupStructure = args(0).getList.map(_.asInstanceOf[NLGroup]).toList
    netLogoSuper ! NetLogoActors.SetGroupStructure(groupStructure)
    
  }
}


//class AddGroup extends DefaultCommand {
//  import QLSystem._
//  
//  override def getAgentClassString = "O"
//  
//  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType))
//  
//  def perform(args: Array[Argument], c: Context) {
//     val agents = args(0).getAgentSet.agents
//     val g = system.actorOf(Props(new QLGroup(netLogoActors, agents.size)))
//     agents.foreach(agent => QLSystem.agentMap(agent) ! QLAgent.AddGroup(g))
//  }
//  
//}


class Start extends DefaultCommand {
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    QLSystem.netLogoSuper ! NetLogoActors.Start
  }
}

class Stop extends DefaultCommand {
    
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    QLSystem.netLogoSuper ! NetLogoActors.Stop
  }
    
}

class DecreaseExperimenting extends DefaultCommand {

  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
//    QLSystem.agentMap.values.foreach(_ ! QLAgent.DecExp)
    QLSystem.qlDataMap.values.foreach(dataAgent => dataAgent send {_.startDecreasing} )
  }
}

/**
 * the UpdateGUI Message should be send to nlguiActor after all previous updates have been made
 * because it triggers a complete change of the alteredData-Agent
 */
//class UpdateGUI extends DefaultCommand {
//  override def getAgentClassString = "O"  
//  override def getSyntax = commandSyntax(Array[Int]())
//  def perform(args: Array[Argument], c: Context) {
//    QLSystem.nlguiActor ! NetLogoActors.UpdateGUI    
//  }
//}

//class GetAlteredData extends DefaultReporter {
//  override def getAgentClassString = "O"    
//  override def getSyntax = reporterSyntax(Array[Int](), ListType)
//  def report(args: Array[Argument], c: Context): AnyRef = { 
////    QLSystem.system.scheduler.scheduleOnce(1000.milliseconds, QLSystem.nlguiActor, NetLogoGUIActor.UpdateGUI)
////    val r = QLSystem.alteredData.await(10.seconds).reverse.map(f => List(f._1, f._2, f._3))
//    implicit val timeout = Timeout(10 seconds)
//    val future = QLSystem.nlguiActor ? NetLogoActors.ReturnData
//    val result = Await.result(future, timeout.duration).asInstanceOf[List[(Agent,String,Double)]]
//    println("result is here: " +  result.size + " items")
//    val r = result.reverse.map(f => List(f._1, f._2, f._3))
//    r.toLogoList
//  }
//}

class GetQValue extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array(StringType), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = {
    val key = args(0).getString
    QLSystem.qlDataMap(c.getAgent).get.qValuesMap.getOrElse(key, new QLAgent.QValue(key, 0.0, 0.0)).value.toLogoObject
  }
}
class GetN extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array(StringType), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = {
    val key = args(0).getString
//    QLSystem.qlDataMap(c.getAgent).get.nMap.getOrElse(key, 0.0).toDouble.toLogoObject
    QLSystem.qlDataMap(c.getAgent).get.qValuesMap.getOrElse(key, new QLAgent.QValue(key, 0.0, 0.0)).n.toLogoObject
  }
}
class GetTotalN extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.qlDataMap(c.getAgent).get.nTotal.toLogoObject
  }
}
class GetLastChoice extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), StringType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.qlDataMap(c.getAgent).get.lastChoice.toLogoObject
  }
}

