package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.Directory
import com.rtjvm.scala.oops.filesystem.State

class Rm(name:String) extends Command{
override def apply(state:State):State={
  val wd=state.wd
  val absolutePath= {
    if (name.startsWith(Directory.SEP)) name
    else if (wd.isRoot) wd.path + name
    else wd.path + Directory.SEP + name
  }
  if(Directory.ROOT_DIR.equals(absolutePath))
    state.setMessage("Root can not be deleted")
  else
    doRm (state, absolutePath)
  }

  def doRm(state: State, path: String):State ={
    def rmHelper(curDir:Directory,path:List[String]): Directory ={
      if(path.isEmpty) curDir
      else if(path.tail.isEmpty) curDir.removeEntry(path.head)
      else{
        val nextDir=curDir.findEntry(path.head)
        if(!nextDir.isDirectory) curDir
        else{
          val newNextDir= rmHelper(nextDir.asDirectory,path.tail)
          if(newNextDir==nextDir) curDir
          else curDir.replaceEntry(path.head,newNextDir)

        }
      }
    }

    val tokens=path.substring(1).split(Directory.SEP).toList
    val newRoot=rmHelper(state.root,tokens)
    if(newRoot==state.root)
      state.setMessage(path+" No such file or directory")
    else
      State(newRoot,newRoot.findDescendant(state.wd.path.substring(1)) )
  }
}
