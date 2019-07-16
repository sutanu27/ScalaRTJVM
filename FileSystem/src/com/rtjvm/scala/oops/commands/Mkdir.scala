package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.{DirEntry, Directory}
import com.rtjvm.scala.oops.filesystem.State

class Mkdir(name:String) extends CreateEntry(name) {
  override def doCreateSpecificEntry (name:String,state:State):DirEntry= {
    print("'"+name+"' Directory has been created")
    Directory.empty(state.wd.path,name)
  }
}
