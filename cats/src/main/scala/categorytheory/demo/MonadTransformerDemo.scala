package categorytheory.demo

import categorytheory.datatypes.MaybeT.transform
import categorytheory.demo.support.UserDomain
import categorytheory.core.ops._

object MonadTransformerDemo extends UserDomain with App {

  def friendsName(userId: String) = for {
    user <- transform <~ findUser(userId)
    friends <- transform <~ findFriends(user)
  } yield friends.map(_.name)

  println(friendsName("user1").value.run(UserRepository))
  println(friendsName("user2").value.run(UserRepository))
  println(friendsName("user3").value.run(UserRepository))
  println(friendsName("user4").value.run(UserRepository))
  println(friendsName("user5").value.run(UserRepository))

}
