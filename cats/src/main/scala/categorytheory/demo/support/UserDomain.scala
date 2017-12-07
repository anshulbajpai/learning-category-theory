package categorytheory.demo.support

import categorytheory.datatypes.{Just, Maybe, Nothing, Reader}

trait UserDomain {

  trait UserRepository {
    private val users = Map(
      "user1" -> User("user1", "user 1"),
      "user2" -> User("user2", "user 2"),
      "user3" -> User("user3", "user 3"),
      "user4" -> User("user4", "user 4")
    )
    private val friends = Map(
      users("user1") -> List(users("user2"), users("user3")),
      users("user2") -> List(users("user1")),
      users("user3") -> List(users("user1"))
    )

    def findUser(userId: String): Maybe[User] = users.get(userId).fold[Maybe[User]](Nothing)(Just(_))
    def findFriends(user: User): List[User] = friends.getOrElse(user, List.empty)
  }

  object UserRepository extends UserRepository

  case class User(id: String, name: String)

  def findUser(userId: String): Reader[UserRepository, Maybe[User]] = Reader(_.findUser(userId))
  def findFriends(user: User): Reader[UserRepository, List[User]] = Reader(_.findFriends(user))

}
