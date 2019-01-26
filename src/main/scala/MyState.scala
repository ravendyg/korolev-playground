import korolev.Context
import korolev.execution._
import scala.concurrent.Future
import korolev.state.javaSerialization._

class MyMessage(
  val author: String,
  val text: String
) { }

case class MyState(
  name: String,
  messages: List[MyMessage]
)

object MyState {
  val globalContext = Context[Future, MyState, Any]
}
