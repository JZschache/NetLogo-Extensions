package de.games

import edu.stanford.multiagent.gamer.Parameters
import edu.stanford.multiagent.gamer.Global
import edu.stanford.multiagent.gamer.ParamParser
import java.util.Random
import java.util.Vector
import java.io.File
import edu.stanford.multiagent.gamer.Game
import com.typesafe.config.ConfigFactory

object GamutGame {
  
  
  /**
   * tested:
   * RandomGame
   * BattleOfTheSexes
   * 
   */
  
  
  def main (args: Array[String]) {
    
    val g = GamutGame("RandomGame", Array(10,11))
        
    println(g.getName())
    println(g.getDescription())
    println(g.getNumPlayers())
    println(g.getNumActions().foldLeft("")((s, e) => s + e + " "))
    println(g.getOutputPayoff(Array(1,1), 0) + " " + g.getOutputPayoff(Array(1,1), 1))
    println(g.getOutputPayoff(Array(1,2), 0) + " " + g.getOutputPayoff(Array(1,2), 1))
    println(g.getOutputPayoff(Array(2,1), 0) + " " + g.getOutputPayoff(Array(2,1), 1))
    println(g.getOutputPayoff(Array(2,2), 0) + " " + g.getOutputPayoff(Array(2,2), 1))
  }
  
  def apply(name: String): Game = apply(name, Array(2,2))
  
  def apply(name: String, actions: Array[Int]): Game = {

    val confFile = "conf/application.conf"
    val config = ConfigFactory.parseFile(new File(confFile))
    val minPayoff = config.getInt("gamut.min-payoff")
    // Make sure that max_payoff is greater than min_payoff
    val maxPayoff = if (minPayoff >= config.getInt("gamut.max-payoff")) minPayoff + 10 else config.getInt("gamut.max-payoff")
    val intMult = config.getInt("gamut.int-mult")
    
    val args = Array("-g", name, "-players", "2", "-actions", actions(0).toString, actions(1).toString, 
        "-normalize", "-min_payoff", minPayoff.toString, "-max_payoff", maxPayoff.toString, 
        "-int_payoffs", "-int_mult", intMult.toString)
    
    val pSeed = new Parameters.ParamInfo("random_seed", Parameters.ParamInfo.LONG_PARAM, 0L, Long.MaxValue, "random seed, uses current time by default.")
    val pGame = new Parameters.ParamInfo("g", Parameters.ParamInfo.VECTOR_PARAM, null, null, "the name of the game to generate, or a list of classes from intersection of which a generator will be picked")
    val pRandomize = new Parameters.ParamInfo("random_params", Parameters.ParamInfo.BOOLEAN_PARAM, null, null, "randomize unset parameters in default ranges",false, false)
    val pOut = new Parameters.ParamInfo("output", Parameters.ParamInfo.STRING_PARAM, null, null, "the name of the outputter to use. (Default: SimpleOutput)",false,"SimpleOutput")
   
    val intPayoffs = new Parameters.ParamInfo("int_payoffs", Parameters.ParamInfo.BOOLEAN_PARAM, null, null, "generate integral payoffs.", false, false)
    val intMultInfo = new Parameters.ParamInfo("int_mult", Parameters.ParamInfo.LONG_PARAM, 1L, 1000000000000L, "a multiplier used before converting payoffs to integers. Defaults to 10000.",false, 10000L)
    val pNormalize = new Parameters.ParamInfo("normalize", Parameters.ParamInfo.BOOLEAN_PARAM, null, null, "use normalization. Note that normalization can result in some error in the last digit of the decimal payoff.", false, false)
    val pMinPayoff = new Parameters.ParamInfo("min_payoff", Parameters.ParamInfo.DOUBLE_PARAM, -Double.MaxValue, Double.MaxValue, "minimum payoff in matrix, set if normalization is desired.", false)
    val pMaxPayoff = new Parameters.ParamInfo("max_payoff", Parameters.ParamInfo.DOUBLE_PARAM, -Double.MaxValue, Double.MaxValue, "maximum payoff in matrix, set if normalization is desired.", false)
    
    val globalParamInfo = Array[Parameters.ParamInfo](pSeed, pGame, pRandomize, pOut,
        intPayoffs, intMultInfo, pNormalize, pMinPayoff, pMaxPayoff)


    Global.params = new Parameters(globalParamInfo)
    Global.gArgs = new Array[String](args.length)
    System.arraycopy(args,0,Global.gArgs,0,args.length)

    var p: ParamParser = null
    try {
      p = new ParamParser(args)
      Global.params.setFromParser(p)
    } catch {
	  case e: Exception =>
	    System.err.println(e.toString())
    }
  
    // -- set the random seed
    Global.randSeed = System.currentTimeMillis()
    Global.rand = new Random(Global.randSeed);

    // -- Instantiate the game
    val gVector = Global.params.getVectorParameter(pGame.name);
    val gName = if(gVector.size() == 1){
	  // -- 1 game only specified
	  gVector.firstElement().asInstanceOf[String];
    } else {
      // -- intersection
      Global.getRandomClassInt(Global.GAME, gVector);
    }
    val g = Global.getObjectOrDie(gName, Global.GAME).asInstanceOf[Game]
    // -- set all parameters and initialize
    try {
      val rp = Global.params.getBooleanParameter(pRandomize.name);
	  g.setParameters(p, rp);
	  g.initialize();
    } catch {
      case e: Exception => {
	    System.err.println("ERROR: Initializing " + gName)
	    System.err.println(e.toString())
      }
    }
    // Generate an instance
    try {
	  g.generate();
    } catch {
      case e: Exception => 
        System.err.println("ERROR: Failed to generate an instance of "+ gName)
        System.err.println(e.toString())
    }
    g
  }
}
