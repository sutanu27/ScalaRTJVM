package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.Files.{DirEntry, FileEntry}
import com.rtjvm.scala.oops.filesystem.State

class Touch(name:String) extends CreateEntry(name) {
  override def doCreateSpecificEntry (name:String,state:State):DirEntry= {
    print("'"+name+"' file has been created")
    FileEntry.empty(state.wd.path,name)
  }
}
