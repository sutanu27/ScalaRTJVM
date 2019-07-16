package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.filesystem.State

class Cat(fileName:String) extends Command {
override def apply(state:State):State = {
  val wd= state.wd
  val dirEntry=wd.findEntry(fileName)
  if(dirEntry==null|| !dirEntry.isFile)
    state.setMessage(fileName+" No such File")
  else
    state.setMessage(dirEntry.asFile.contents)
}
}
