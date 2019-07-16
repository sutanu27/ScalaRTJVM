package com.rtjvm.scala.oops.Files

import com.rtjvm.scala.oops.filesystem.FilesystemException

class Directory(override val parentPath: String,override val name: String , val contents: List[DirEntry] ) extends DirEntry(parentPath,name )
{
  def isRoot: Boolean=parentPath.isEmpty
  def isDirectory: Boolean= true
  def isFile: Boolean=false
  def getType: String ="Directory"
  def asDirectory: Directory = this
  def asFile: FileEntry = throw new FilesystemException("Directory can not be changed as File")
  def hasEntry(name : String): Boolean= findEntry(name)!=null
  def findDescendant(path:List[String]):Directory= {
    if(path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)
  }
  def findDescendant(path:String): Directory= {
    if(path.isEmpty) this
    else findDescendant(path.split(Directory.SEP).toList)
  }
  def removeEntry(ename:String): Directory= {
    if(!hasEntry(ename)) this
    else {
      println(ename+" has been removed")
      new Directory(parentPath,name, contents.filter(x => !x.name.equals(ename)))
    }
  }
  def addEntry(newEntry:DirEntry): Directory= new Directory(parentPath,name,contents :+ newEntry)
  def findEntry(entryName:String): DirEntry= {
    def findHelper(name:String, contents:List[DirEntry]): DirEntry={
      if(contents.isEmpty) null
      else if (name.equals(contents.head.name)) contents.head
      else findHelper(name,contents.tail)
    }
    findHelper(entryName,contents)
  }
  def replaceEntry(entry:String, newEntry:DirEntry)= new Directory(parentPath,name,contents.filter(e => !e.name.equals(entry)) :+newEntry)
}


object Directory{
  val SEP="/"
  val ROOT_DIR="/"
  def ROOT: Directory=empty("","")
  def empty(parentPath:String, name:String): Directory= new Directory(parentPath,name,List())
}