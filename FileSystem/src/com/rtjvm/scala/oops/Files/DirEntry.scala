package com.rtjvm.scala.oops.Files

abstract class DirEntry(val parentPath: String, val name: String ) {
  def asDirectory: Directory
  def asFile: FileEntry
  def isDirectory: Boolean
  def isFile:Boolean
  def getAllFoldersInPath : List[String]= {
    path.substring(1).split(Directory.SEP).toList.filter(x => !x.isEmpty)
  }
  def path: String={
    if(parentPath.equals("/"))Directory.SEP+name
  else parentPath+Directory.SEP+name
  }

def getType : String
}
