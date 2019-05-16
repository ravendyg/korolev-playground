import korolev.Context
import korolev.execution._
import scala.concurrent.Future
import korolev.state.javaSerialization._

class XmlDoc(
  val text: String,
  val expanded: Boolean,
  val children: List[XmlDoc],
) {}

class MyMessage(
  val author: String,
  val text: String
) { }

case class MyState(
  tab: Int,
  xmls: List[XmlDoc],
)

object MyState {
  val globalContext = Context[Future, MyState, Any]
}
