package exercises

object Network extends App{

  def add(net:Map[String,Set[String]],person:String): Map[String,Set[String]] = {
    net+(person->Set())
  }

  def remove(net:Map[String,Set[String]],person:String) :Map[String,Set[String]] = {
    def reloveHelper(newnet:Map[String,Set[String]],a:String,friendL:Set[String]):Map[String,Set[String]]={
    if(friendL.isEmpty) newnet
    else {
      reloveHelper(unfriend(newnet,a,friendL.head),a,friendL.tail)
    }
    }
    reloveHelper(net,person,net(person)) - person
  }


  def friend(net:Map[String,Set[String]],a:String,b:String):Map[String,Set[String]] = {
    val frienda =net(a)
    val friendb =net(b)
    net+(a->(frienda + b))+(b->(friendb + a))
  }
  def unfriend(net:Map[String,Set[String]],a:String,b:String):Map[String,Set[String]] = {
    val frienda =net(a)
    val friendb =net(b)
    net+(a->(frienda - b))+(b->(friendb - a))
  }

 var FB:Map[String,Set[String]]=Map()
  FB=add(FB,"Suta")
  FB=add(FB,"Raju")
  FB=add(FB,"Hari")
  FB=add(FB,"Soum")
  FB=add(FB,"Avi")
  FB=add(FB,"Amri")
  FB=friend(FB,"Suta","Hari")
  FB=friend(FB,"Raju","Hari")
  FB=friend(FB,"Suta","Raju")
  FB=friend(FB,"Suta","Soum")
  FB=friend(FB,"Suta","Raju")
  print(ZeroFriends(FB))

def nFriend(net:Map[String,Set[String]],a:String):Int={
  if(net(a).isEmpty)0
  else net(a).size
}

  def most(net:Map[String,Set[String]]):String={
    def mostHelper(Newnet:Map[String,Set[String]],max:Int, name:String):String={
      if(Newnet.isEmpty) "This Newtork is Empty"
      else if(Newnet.tail.isEmpty) name
      else {
        val nMax=nFriend(Newnet,(Newnet.head._1))
        val nName=Newnet.head._1
        if(nMax>max) mostHelper(Newnet.tail,nMax,nName)
        else mostHelper(Newnet.tail,max,name)
      }
    }
    mostHelper(net,0,"")
  }
  def ZeroFriends(net:Map[String,Set[String]]): Int={
    net.filter((x:(String,Set[String])) => x._2.isEmpty).size
  }

  def socialCon(net:Map[String,Set[String]],a:String, b:String): Boolean= ???
}
