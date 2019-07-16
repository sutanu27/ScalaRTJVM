package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.filesystem.State

class UnknownCommand extends Command {
override def apply(state: State): State=state.setMessage("Invalid Command")
}
