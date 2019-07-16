package com.rtjvm.scala.oops.filesystem
import java.util.Scanner

import com.rtjvm.scala.oops.Files.Directory
import com.rtjvm.scala.oops.commands.Command

object Filesystem extends App {
  val root =Directory.ROOT
  var state=State(root,root)
  val input= new Scanner(System.in)
while(true){
  state.show()
  val ip=input.nextLine()
  state=Command.from(ip).apply(state)
}
}
