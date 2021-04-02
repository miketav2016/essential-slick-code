// Import the Slick interface for H2:
import slick.jdbc.H2Profile.api._

import scala.concurrent.Await
import scala.concurrent.duration._

object Example extends App {

  // Case class representing a row in our table:
  final case class Message(
    sender:  String,
    content: String,
    id:      Long = 0L)

  // Helper method for creating test data:
  def freshTestData = Seq(
    Message("Dave", "Hello, HAL. Do you read me, HAL?"),
    Message("HAL",  "Affirmative, Dave. I read you."),
    Message("Dave", "Open the pod bay doors, HAL."),
    Message("HAL",  "I'm sorry, Dave. I'm afraid I can't do that."),
    Example.Message("Mike", "Slick it's cool")
  )

  // Schema for the "message" table:
  final class MessageTable(tag: Tag)
      extends Table[Message](tag, "message") {

    def id      = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def sender  = column[String]("sender")
    def content = column[String]("content")

    def * = (sender, content, id).mapTo[Message]
  }

  // Base query for querying the messages table:
  lazy val  messages = TableQuery[MessageTable]
  // messages: TableQuery[MessageTable] = Rep(TableExpansion)

  // An example query that selects a subset of messages:
  val halSays = messages.filter(_.sender === "HAL")

  val halSays2 = for {
    message <- messages if message.sender === "HAL"
  } yield message

  val actions: DBIO[Seq[Message]] = (
      messages.schema.create andThen
          (messages ++= freshTestData) andThen
          halSays.result
      )
  val sameActions: DBIO[Seq[Message]] = (
      messages.schema.create >>
          (messages ++= freshTestData) >>
          halSays.result
      )


  // Create an in-memory H2 database;
  val db = Database.forConfig("chapter01")

  // Helper method for running a query in this example file:
  def exec[T](program: DBIO[T]): T = Await.result(db.run(program), 2 seconds)

  // Create the "messages" table:
  println("Creating database table")
  exec(messages.schema.create)
//  messages.schema.createStatements.mkString
//  val action: DBIO[Unit] = messages.schema.create
//  val future: Future[Unit] = db.run(action)
//  val result = Await.result(future, 2.seconds)

  // Create and insert the test data:
  println("\nInserting test data")
  exec(messages ++= freshTestData)

  // Run the test query and print the results:
  println("\nSelecting all messages:")
  exec( messages.result ) foreach { println }

  println("\nSelecting only messages from HAL:")
  exec( halSays.result ) foreach { str => println(str) }
  exec( halSays2.result ) foreach { str => println(str) }

  println("111")


//  val insert: DBIO[Option[Int]] = messages ++= freshTestData
//  val insertAction: Future[Option[Int]] = db.run(insert)
}
