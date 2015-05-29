package de.qlearning


import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import org.nlogo.app.App



object StartNetLogo {
  
  def main (args: Array[String]) {
    
    // starting the NetLogo GUI
    App.main(args)
    
    // starting the QLSystem
    QLSystem.init
    
  }  
}