package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.{Directory, FileEntry}
import com.rtjvm.scala.oops.filesystem.State

class Echo(arg:Array[String]) extends Command {
  def apply(state:State):State= {
    if(arg.isEmpty) state
    else if(arg.length==1) state.setMessage(arg(0))
    else{
      val operator=arg(arg.length-2)
      val file=arg(arg.length-1)
      val content=CreateContent(arg,arg.length-2)
      if(operator.equals(">>"))
        writeFile(state, file ,content,true)
      else if(operator.equals(">"))
        writeFile( state, file ,content,false)
      else
        state.setMessage(CreateContent(arg,arg.length))
    }
  }

  def CreateContent(strings: Array[String], length:Int): String = {
    def CreateContentHelper(curIndex:Int, Result:String): String={
      if(curIndex>=length) Result
      else CreateContentHelper(curIndex+1,Result+" "+strings(curIndex))
    }
    CreateContentHelper(0,"")
  }

  def getRootAfterEcho(curDir:Directory,path:List[String],contents:String,append:Boolean): Directory ={
    if(path.isEmpty) curDir
    else if(path.tail.isEmpty)
      {
        val dirEntry=curDir.findEntry(path.head)
        if(dirEntry==null) curDir.addEntry(new FileEntry(curDir.path,path.head,contents))
        else if(dirEntry.isDirectory) curDir
        else if(append) curDir.replaceEntry(path.head, dirEntry.asFile.appendContent(contents))
        else  curDir.replaceEntry(path.head, dirEntry.asFile.setContent(contents))
      }
    else {
      val nextDir=curDir.findEntry(path.head).asDirectory
      val newNextDir=getRootAfterEcho(nextDir,path.tail,contents,append)
      if(nextDir==newNextDir) curDir
      else curDir.replaceEntry(path.head,newNextDir)
    }
  }

  def writeFile(state:State ,fileName: String, Contents: String, append:Boolean):State = {
    if(fileName.contains(Directory.SEP)) state.setMessage("Echo: Filename must not contains Separator")
    else {
      val newRoot:Directory= getRootAfterEcho(state.root,state.wd.getAllFoldersInPath :+ fileName,Contents,append )
      if(newRoot==state.root)
        state.setMessage("No Such File "+fileName)
      else
        State(newRoot,newRoot.findDescendant(state.wd.getAllFoldersInPath))
    }
  }
}
