package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.DirEntry
import com.rtjvm.scala.oops.filesystem.State

class Ls extends Command {
override def  apply(state: State): State={
  val content=state.wd.contents
  val niceOutput=createNiceOutput(content)
  state.setMessage(niceOutput)
}
  def createNiceOutput(content: List[DirEntry]): String ={
  if(content.isEmpty)""
    else{
    val entry=content.head
    entry.name+"["+entry.getType+"]"+"\n"+createNiceOutput(content.tail)
  }
  }
}
