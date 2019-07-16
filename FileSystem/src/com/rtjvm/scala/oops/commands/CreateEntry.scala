package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.{DirEntry, Directory}
import com.rtjvm.scala.oops.filesystem.State

abstract class CreateEntry(name:String) extends Command {
  override def apply(state: State): State = {
    val wd=state.wd
    if (wd.hasEntry(name)) {
      state.setMessage("Entry with '"+name+"' name already exists.")
    }
    else if (name.contains(Directory.SEP)) {
      state.setMessage("Entry name contains separators.")
    }
    else if (checkIllegal(name)){
      state.setMessage("Entry name is illegal.")
    }
    else doCreateEntry(state,name)
  }
  def checkIllegal(str: String):Boolean= str.contains(".")

  def doCreateEntry(state:State, str: String): State= {
    val wd=state.wd
    val allDirInPath=wd.getAllFoldersInPath
    val newEntry= doCreateSpecificEntry(name,state)
    val newRoot= updateStructure(state.root,allDirInPath,newEntry)
    val newWd=newRoot.findDescendant(allDirInPath)
    State(newRoot,newWd)
  }

  def doCreateSpecificEntry (name:String,state:State):DirEntry

  def updateStructure(currentDir:Directory, path: List[String],newDir:DirEntry):Directory={
    if(path.isEmpty) currentDir.addEntry(newDir)
    else{
    //  print(path)
    //  println(currentDir.findEntry(path.head))
      val oldEntry=currentDir.findEntry(path.head).asDirectory
      currentDir.replaceEntry(oldEntry.name,updateStructure(oldEntry,path.tail,newDir))
    }

  }
}
