import StructureExample.{HighPriority, LowPriority, Priority}
import slick.jdbc.JdbcProfile

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

trait Profile {
  val profile : JdbcProfile
}

trait Tables {
  // Self-type indicating that our tables must be mixed in with a Profile
  this: Profile =>

  // Whatever that Profile is, we import it as normal:
  import profile.api._

  // Row and table definitions here as normal
  case class Message(sender: String, content: String, id: Long = 0L)

  final class MessageTable(tag: Tag) extends Table[Message](tag, "message") {
    def id      = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def sender  = column[String]("sender")
    def content = column[String]("content")
    def * = (sender, content, id).mapTo[Message]
//    def * = (sender, content, id)<>(Message.tupled, Message.unapply)
  }

  object messages extends TableQuery( new MessageTable(_)) {
    def messagesFrom(name: String) = this.filter(_.sender === name)
    val numSenders = this.map(_.sender).distinct.length
  }

  //4 Mapping Enumeraঞons
  object UserRole extends Enumeration {
    type UserRole = Value
    val Owner = Value("O")
    val Regular = Value("R")
  }

  import UserRole._

  case class User(name: String,
                  email: Option[String],
                  id: Long = 0,
                  userRole: UserRole = Regular,
                  priority: Priority = HighPriority)

  implicit val userRoleMapped = MappedColumnType.base[UserRole, String](_.toString, UserRole.withName(_))
  //
//  implicit val userRoleMappedInt = MappedColumnType.base[UserRole, Int](_.id, v =>
//    UserRole.values.find(_.id == v).getOrElse(UserRole.Regular))

  implicit val priorityType =
    MappedColumnType.base[Priority, String](
      flag => flag match {
        case HighPriority => "y"
        case LowPriority => "n"
      }, str => str match {
        case "Y" | "y" | "+" | "high" => HighPriority
        case "N" | "n" | "-" | "lo" | "low" => LowPriority
      }
    )

  class UserTable(tag: Tag) extends Table[User](tag, "filtering_3") {

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def email = column[Option[String]]("email")
    def userRole = column[UserRole]("userRole", O.Length(1, varying = false))
    def priority = column[Priority] ("priority")

    def * = (name, email, id).mapTo[User]
  }

  lazy val users = TableQuery[UserTable]
  val setup = DBIO.seq(
    users.schema.create,
    users += User("Dave", Some("dave@example.org")),
    users += User("HAL ", None)
  )
}




object StructureExample extends App {

  // Bring all the components together:
  class Schema(val profile: JdbcProfile) extends Tables with Profile

  // A specific schema with a particular driver:
  val schema = new Schema(slick.jdbc.H2Profile)

  // Use the schema:
  import schema._
  import profile.api._

  val db = Database.forConfig("chapter05")

  // Insert the conversation
  val msgs = Seq(
      Message("Dave", "Hello, HAL. Do you read me, HAL?"),
      Message("HAL",  "Affirmative, Dave. I read you."),
      Message("Dave", "Open the pod bay doors, HAL."),
      Message("HAL",  "I'm sorry, Dave. I'm afraid I can't do that.")
    )

  val program = for {
    _ <- messages.schema.create
    _ <- messages ++= msgs
    c <- messages.numSenders.result
    } yield c

  val result = Await.result(db.run(program), 2 seconds)
  println(s"Number of senders $result")


  //1 Filtering Optional Columns
  def filterByEmail(email: Option[String]): Query[schema.UserTable, schema.User, Seq] = {
    email match {
      case None => users
      case Some(_) => users.filter(_.email === email)
    }
  }
  //2 Matching or Undecided
  def filterByEmailMod1(email: Option[String]): Query[schema.UserTable, schema.User, Seq] = {
    users.filter(user => user.email === email || user.email.isEmpty )
  }
  //3 Enforcement
  //4 Mapping Enumeraঞons
  //5 Alternaঞve Enumeraঞons
  //6 Custom Boolean
  sealed trait Priority
  case object HighPriority extends Priority
  case object LowPriority extends Priority

  //7 Turning a Row into Many Case Classes




}
