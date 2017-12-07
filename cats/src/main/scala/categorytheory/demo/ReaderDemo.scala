package categorytheory.demo

import categorytheory.core.ops._
import categorytheory.datatypes.Reader
import categorytheory.demo.support.UserDomain

object ReaderDemo extends UserDomain with App {

  def friendsName(userId: String): Reader[UserRepository, List[String]] = for {
    user <- findUser(userId)
    friends <- user.map(findFriends).getOrElse(Reader.value(List.empty[User]))
  } yield friends.map(_.name)


  println(friendsName("user1").run(UserRepository))
  println(friendsName("user2").run(UserRepository))
  println(friendsName("user3").run(UserRepository))
  println(friendsName("user4").run(UserRepository))
  println(friendsName("user5").run(UserRepository))

}
