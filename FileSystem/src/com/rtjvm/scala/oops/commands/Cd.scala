package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.{DirEntry, Directory}
import com.rtjvm.scala.oops.filesystem.State

import scala.annotation.tailrec

class Cd(val dir:String) extends Command {
  def apply(state:State):State ={

    val root=state.root
    val wd=state.wd

    val absolutePath={
      if(dir.startsWith(Directory.SEP)) dir
      else if(wd.isRoot) wd.path+dir
      else wd.path+Directory.SEP+dir
    }
    val destinationDir= doFindEntry(root,absolutePath)

    if (destinationDir==null)
      state.setMessage("Invalid 'cd' Command")
    else if(!destinationDir.isDirectory)
      state.setMessage("'"+dir+"' No such directory")
    else
      State(root,destinationDir.asDirectory)
  }

  def doFindEntry(root:Directory, absolutePath: String): DirEntry={

    def colapsePath(tokens:List[String], Result:List[String]):List[String]={
    if(tokens.isEmpty) Result
    else if(tokens.head.equals(".")) colapsePath(tokens.tail,Result)
    else if(tokens.head.equals(".."))
    {
      if(Result.isEmpty) null
      else colapsePath(tokens.tail,Result.init)
    }
    else colapsePath(tokens.tail,Result :+ tokens.head )
    }

    @tailrec
    def doFindEntryHelper(dir:Directory, path:List[String]): DirEntry ={
      if(path.isEmpty|| path.head.isEmpty) dir
      else if (path.tail.isEmpty) dir.findEntry(path.head)
      else {
        val nextDir=dir.findEntry(path.head)
        if(nextDir==null || !nextDir.isDirectory) null
        else doFindEntryHelper(nextDir.asDirectory,path.tail)
      }
    }
    val tokens=absolutePath.substring(1).split(Directory.SEP).toList
    val newTokens=colapsePath(tokens,List())

    if (newTokens==null) null
    else
    doFindEntryHelper(root,newTokens)
  }
}
