package com.rtjvm.scala.oops.filesystem

import com.rtjvm.scala.oops.Files.Directory

class State(val root: Directory, val wd:Directory,val output:String) {

def show(): Unit={
  println(output)
  print(State.SHELL_TOKEN)
}
  def setMessage(msg:String): State=State(root,wd,msg)
}

object State{
  val SHELL_TOKEN="$ "
   def apply(root: Directory, wd:Directory,output:String=""): State= new State(root,wd,output )
}