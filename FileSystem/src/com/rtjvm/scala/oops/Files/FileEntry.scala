package com.rtjvm.scala.oops.Files

import com.rtjvm.scala.oops.filesystem.FilesystemException

class FileEntry(override val parentPath: String, override val name: String, val contents: String) extends DirEntry(parentPath,name) {
  def asDirectory: Directory = throw new FilesystemException("File can not be changed as directory")
  def asFile: FileEntry = this
  def getType : String = "File"
  def isDirectory: Boolean= false
  def isFile: Boolean=true
  def setContent(contents: String)=  new FileEntry(parentPath,name,contents)
  def appendContent(content: String)= setContent(contents+"\n"+content)
}
object FileEntry{
  def empty(parentPath:String, name:String): FileEntry= new FileEntry(parentPath,name,"")
}