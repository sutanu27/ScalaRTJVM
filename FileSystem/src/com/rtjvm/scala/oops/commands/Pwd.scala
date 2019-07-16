package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.filesystem.State

class Pwd extends Command {
  override def apply(state:State)= state.setMessage(state.wd.path)
}
