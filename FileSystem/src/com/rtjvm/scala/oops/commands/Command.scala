package com.rtjvm.scala.oops.commands

import com.rtjvm.scala.oops.filesystem.State

trait Command {
def apply(state:State):State
}

object Command{
  val Mkdir="mkdir"
  val Touch="touch"
  val Ls="ls"
  val Pwd="pwd"
  val Cd="cd"
  val Rm="rm"
  val Echo="echo"
  val Cat="cat"

  def emptyCommand: Command=new Command{
    override def apply(state:State): State=state
  }

  def incompleteCommand(com:String): Command=new Command{
    override def apply(state:State): State=state.setMessage(com+" : is an invalid Command")
  }

  def from(input: String): Command={
    val tokens=input.split(" ")
    if(tokens.isEmpty||input.isEmpty) emptyCommand
    else if(tokens(0).equals(Mkdir))
    {
      if (Mkdir.equals(tokens(0)) && tokens.length<2) incompleteCommand(Mkdir)
      else new Mkdir(tokens(1))
    }
    else if(tokens(0).equals(Touch))
      {
        if (Touch.equals(tokens(0)) && tokens.length<2) incompleteCommand(Touch)
        else new Touch(tokens(1))

      }
    else if(tokens(0).equals(Ls))
      {
        new Ls
      }
    else if(tokens(0).equals(Pwd))
      {
        new Pwd
      }
    else if(tokens(0).equals(Cd)){
      if (Cd.equals(tokens(0)) && tokens.length<2) incompleteCommand(Cd)
      else new Cd(tokens(1))
    }
    else if(tokens(0).equals(Rm)){
      if (Rm.equals(tokens(0)) && tokens.length<2) incompleteCommand(Rm)
      else new Rm(tokens(1))
    }
    else if(tokens(0).equals(Echo)){
      if (Echo.equals(tokens(0)) && tokens.length<2) emptyCommand
      else new Echo(tokens.tail)
    }
    else if(tokens(0).equals(Cat)){
      if (Cat.equals(tokens(0)) && tokens.length<2) incompleteCommand(Cat)
      else new Cat(tokens(1))
    }

    else new UnknownCommand
  }
}